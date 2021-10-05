package tsparse

import fastparse._, JavaWhitespace._
import java.io.File
import scala.io.Source
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

object Main extends App {
  type FileResult = (Seq[String], Seq[TopLevelStatement])

  def getFileText(file: File): String = {
    val src = Source.fromFile(file)
    val filePath = file.getPath()
    val string =
      try src.mkString
      finally src.close()
    Preprocessors(filePath, string)
  }

  def writeFileText(name: String, text: String): Unit = {
    val processedText = Postprocessors(name, text)
    val f = new File(name)
    f.getParentFile().mkdirs()
    f.createNewFile()
    val writer = new java.io.PrintWriter(f)
    try {
      writer.println(processedText)
    } finally {
      writer.close()
    }
  }

  // Not strictly optimized per se; but then we're not running this a lot.
  def parseFile(path: Seq[String], file: File): FileResult = {
    val string = getFileText(file)
    val parseResult = Parser.run(string)
    parseResult match {
      case f: Failure => {
        throw new Exception(f.trace().longMsg)
      }
      case Success(value, index) => {
        assert(index == string.length())
        (path, value)
      }
    }
  }

  def parseDirectory(path: Seq[String], file: File): Seq[FileResult] = {
    if (!file.isDirectory() && file.getName().endsWith(".d.ts")) {
      Seq(parseFile(path, file))
    } else if (!file.isDirectory()) {
      Seq()
    } else {
      val deeperPath = path :+ file.getName()
      val files = file.listFiles()
      if (files == null) Seq() else files.toSeq.flatMap(f => parseDirectory(deeperPath, f))
    }
  }

  def parseRootDirectory(path: Seq[String], file: File): Seq[FileResult] = {
    // Assuming this is, in fact, a directory.
    file.listFiles().toSeq.flatMap(f => parseDirectory(path, f))
  }

  def assignOverrides(
      member: SJSTopLevel,
      allMembers: Map[String, SJSTopLevel], // Already exists
      memberCache: Map[String, Set[String]]
  ): Unit = {
    if (memberCache contains member.name) return

    def nameOfType(t: DataType): Option[String] = {
      t match {
        case Base(name)             => Option(name)
        case Parameterized(name, _) => Option(name)
        case _                      => None
      }
    }

    member match {
      case comp: Composite => {
        val parentNames = comp.extensions.map(exts => exts.flatMap(nameOfType)).getOrElse(Seq()).toSet

        for (parent <- parentNames)
          allMembers
            .get(parent)
            .map(parentMember => assignOverrides(parentMember, allMembers, memberCache))
        val parentMembers = parentNames.flatMap(p => memberCache.getOrElse(p, Seq()))

        val cachedMembers = comp.members.flatMap {
          case o: Overridable => {
            if (parentMembers contains o.name) o.overrides = true
            Some(o.name)
          }
          case _ => None
        }
        memberCache.update(comp.name, parentMembers ++ cachedMembers)
      }
      case _ => ()
    }
  }

  def dedupCompanions(
      members: Seq[SJSTopLevel],
      companions: Seq[CompanionObject]
  ): Buffer[SJSTopLevel] = {
    val result = members.toBuffer
    for (companion <- companions) {
      val conflictingMembers = members.filter(_.name == companion.name).flatMap {
        case obj: NativeObject => Some(obj)
        case _                 => None
      }
      assert(conflictingMembers.length <= 1)
      if (conflictingMembers.length == 1) {
        conflictingMembers(0).members ++= companion.members
      } else {
        result += companion
      }
    }
    result
  }

  def transform(results: Seq[FileResult]): (Module, TransformContext) = {
    implicit val transformContext = new TransformContext("")

    val rootModule = new Module("three")

    def getOrCreateModule(path: Seq[String], root: Module = rootModule): Module = {
      def newModule(name: String) = new Module(name)
      path.foldLeft(root) { case (mod, name) =>
        mod.subModules.getOrElseUpdate(name, newModule(name))
      }
    }

    val allMembers = Map[String, SJSTopLevel]()

    for (result <- results) {
      val (path, tree) = result
      val module = getOrCreateModule(path)

      val transformedMembers = tree.map(_.transform)
      val dedupMembers = dedupCompanions(transformedMembers, transformContext.resetCompanions())
      module.members ++= dedupMembers
      allMembers ++= transformedMembers.map(mem => (mem.name, mem))
    }

    val memberCache = Map[String, Set[String]]()
    for (member <- allMembers.values) {
      assignOverrides(member, allMembers, memberCache)
    }

    (rootModule, transformContext)
  }

  val headers = Lines(
    "\n",
    "import scala.scalajs.js",
    "import js.annotation.*",
    "import org.scalajs.dom.*",
    "import org.scalajs.dom.raw.{HTMLElement, HTMLDocument, HTMLMediaElement, HTMLVideoElement, HTMLCanvasElement, HTMLImageElement}",
    "import org.scalajs.dom.raw.{WebGLShader, WebGLFramebuffer}",
    "import org.scalajs.dom.experimental.gamepad.*",
    "import scalajs.js.typedarray.*",
    ""
  ).emit()

  // Usually imports from TS Stdlib
  val globalDefintions = Lines(
    "type Record[K,V] = js.native",
    "type WebGLBuffer = js.native",
    "type MediaStream = js.native",
    "type ImageBitmap = js.native",
    "type BufferSource = js.native",
    "type MimeType = js.native",
    "type WebGL2RenderingContext = js.native",
    "type DOMPointReadOnly = js.native",
    "type DOMHighResTimeStamp = js.native",
    "\n"
  ).emit()

  def emit(
      outDir: String,
      currentImport: String,
      inputs: (Module, TransformContext),
      existingModules: Seq[String] = Seq()
  ) = {
    implicit val currentPath = currentImport

    val (root, transformContext) = inputs
    val allModules = root.flattenModules()
    val moduleNames = existingModules ++ allModules.map(_.name)
    println(allModules.map(_.name))

    def imports(currentModule: String) =
      new Lines(
        moduleNames
          .filter(_ != currentModule)
          .map(mod => s"import typings.$mod.*")
      ).pad()

    def globals(currentModule: String) =
      if (currentModule == "three") globalDefintions else ""

    implicit val ctx = new Emitter.TypeContext()
    ctx.defaultTypeArgs = transformContext.typeMapping

    for (module <- allModules) {
      val packageName = s"package typings.${module.name}"
      val fileName = module.name.replace('.', '/')

      val currentImports = imports(module.name).emit()
      val currentGlobals = globals(module.name)

      val firstMembers = module.members
        .map(member => { member.jsName = s"${member.name}"; member })
        .map(_.emit.emit())
      val interfaceTypes = ctx.resetTypes().map(_.emit.emit())
      val members = firstMembers ++ interfaceTypes

      if (members.length > 0) {
        val fileText =
          packageName + headers + currentImports + currentGlobals + members.mkString("\n")

        writeFileText(s"$outDir/$fileName.scala", fileText)
      }
    }

    moduleNames
  }

  // Don't write out the files, just get the module names.
  def virtualEmit(outDir: String, inputs: (Module, TransformContext)) = {
    val (root, transformContext) = inputs
    val allModules = root.flattenModules()
    allModules.map(_.name)
  }

  val coreModules = emit(
    outDir = "/Users/dan/repos/scala-threejs-facades/src/main/scala",
    currentImport = "three",
    inputs = transform(parseRootDirectory(Seq(), new File("data/src")))
  )

  def emitExample(path: String) = {
    val pathList = Seq("examples") ++ path.split("/").dropRight(1)
    val importPath = s"three/examples/jsm/${path.split('.')(0)}"
    emit(
      outDir = "/Users/dan/repos/scala-threejs-facades/src/main/scala",
      currentImport = importPath,
      transform(parseDirectory(pathList, new File(s"data/examples/jsm/$path"))),
      coreModules
    )
  }

  emitExample("controls/OrbitControls.d.ts")

}
