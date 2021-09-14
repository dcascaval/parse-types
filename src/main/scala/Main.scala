package tsparse

import fastparse._, JavaWhitespace._
import java.io.File
import scala.io.Source
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

// Use this when there's some typescript construct we don't know how to support in scala,
// so instead we string match.
// Currently we also make use of it when there's an edge case that appears literally once
// in the target library that we don't want to modify our code to support.
case class Preprocessor(file: String, oldText: String, newText: String) {
  def matches(targetFilePath: String) = targetFilePath.contains(file)
  def replace(targetText: String) = targetText.replace(oldText, newText)
  def apply(targetFilePath: String, targetText: String) =
    if (matches(targetFilePath)) replace(targetText) else targetText
}

// FEATURES TO REMOVE THE HACKS:
//  - match types
//  - JSCallable annotation
//  - interface indexing
//  - readonly parsing hack (don't parse it as `readonly` token if there's a colon after it)
//  - tuple types
//  - default parsing hack (`export default` to be followed by toplevel stmt.)

// To elevate this to the quality where it's worth distributing, we should additionally:
// - Address export handling correctly instead of assuming everything to be visible
// - Parse doc comments so that they end up alongside their corresponding members.

object Main extends App {

  type FileResult = (Seq[String], Seq[TopLevelStatement])

  val preprocessors = Seq(
    // Not currently supporting conditional types, and there is only seemingly one instance in the definitions.
    Preprocessor(
      file = "src/renderers/webgl/WebGLCubeUVMaps.d.ts",
      oldText = "get<T>(texture: T): T extends Texture ? Texture : T;",
      newText = "get<T extends Texture>(texture: T): T;"
    ),
    // This completely breaks the parser, and we cannot allow empty idents.
    Preprocessor(
      file = "data/examples/jsm/libs/fflate.module.min.d.ts",
      oldText = "    (): void;",
      newText = ""
    ),
    // Instead of implementing our own type indexing system, just replace with this.
    Preprocessor(
      file = "data/examples/jsm/libs/fflate.module.min.d.ts",
      oldText = "GzipOptions['mtime'] | undefined",
      newText = "Date | string | number | undefined"
    ),
    // Typescript allows their keywords to be identifiers, for JS compat.
    Preprocessor(
      file = "data/examples/jsm/nodes/core/InputNode.d.ts",
      oldText = "readonly: boolean;",
      newText = "readonly readonly: boolean;"
    ),
    // It's either empty or a Uint8Array. However, we have no equivalent type in Scala.
    Preprocessor(
      file = "data/examples/jsm/exporters/MMDExporter.d.ts",
      oldText = "[] | Uint8Array",
      newText = "Uint8Array"
    ),
    // There is only a single instance of this in the entire definitions.
    Preprocessor(
      file = "data/examples/jsm/csm/Frustum.d.ts",
      oldText = "export default class",
      newText = "export class"
    )
  )

  def getFileText(file: File): String = {
    val src = Source.fromFile(file)
    val filePath = file.getPath()
    val string =
      try src.mkString
      finally src.close()
    preprocessors.foldLeft(string)((current, processor) => processor(filePath, current))
  }

  def writeFileText(name: String, text: String): Unit = {
    println(s"Writing ${name}")

    val f = new File(name)
    f.getParentFile().mkdirs()
    f.createNewFile()
    val writer = new java.io.PrintWriter(f)
    try {
      writer.println(text)
    } finally {
      writer.close()
    }
  }

  // Not strictly optimized per se; but then we're not running this a lot.
  def parseFile(path: Seq[String], file: File): FileResult = {
    // println(s"Parsing ${file.getPath()}")
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

  // parseDirectory(Seq("examples"), new File("data/examples"))
  // parseDirectory(Seq(), new File("data/src"))

  def transform(results: Seq[FileResult]): Seq[SJSTopLevel] = {
    implicit val transformContext = new TransformContext("")

    val rootModule = new NativeObject("GLOBAL", "")

    def getOrCreateModule(path: Seq[String], root: NativeObject = rootModule): NativeObject = {
      def newModule(name: String) = new NativeObject(name, s"THREE", Buffer(), true)
      path.foldLeft(rootModule) { case (mod, name) =>
        mod.subObjects.get(name) match {
          case Some(value) => value
          case None => {
            val result = newModule(name)
            mod.addMembers(Seq(result))
            result
          }
        }
      }
    }

    for (result <- results) {
      val (path, tree) = result
      val module = getOrCreateModule(path)
      module.addMembers(tree.map(_.transform))
      module.addMembers(transformContext.resetCompanions())
    }

    rootModule.getMembers
  }

  val headers = Lines(
    "package typings.three",
    "",
    "import scala.scalajs.js",
    "import js.annotation.*",
    "import org.scalajs.dom.*",
    "import scalajs.js.typedarray.*",
    ""
  ).emit()

  val globalDefintions = Lines(
    "type ArrayLike[T] = js.native;"
  )

  def emit(outDir: String, stmts: Seq[SJSTopLevel]) = {

    def allModuleNames(stmt: SJSTopLevel, prefix: Option[String] = None): Seq[String] = {
      stmt match {
        case a: NativeObject if a.isModule => {
          val nextPrefix = prefix.map(p => s"$p.${a.name}").getOrElse(a.name)
          Seq(nextPrefix) ++ a.subObjects.values.flatMap(so => allModuleNames(so, Some(nextPrefix)))
        }
        case _ => Seq()
      }
    }

    def getModuleName(stmt: SJSTopLevel): Option[String] = {
      stmt match {
        case a: NativeObject if a.isModule => Some(a.name)
        case _                             => None
      }
    }

    val allModules = stmts.flatMap(s => allModuleNames(s))
    val moduleNames = stmts.flatMap(getModuleName)
    val fileNames = Seq("three") ++ moduleNames
    val files: Map[String, Buffer[Lines]] = Map.from(fileNames.map(name => (name, Buffer[Lines]())))

    def imports(currentModule: String) =
      new Lines(
        allModules
          .filter(_ != currentModule)
          .map(mod => s"import typings.three.$mod.*")
      ).pad()

    implicit val ctx = new Emitter.TypeContext()

    for (obj <- stmts) {
      val fileName = getModuleName(obj).getOrElse("three")
      files(fileName) += obj.emit
      files(fileName) += new Lines(Seq(), ctx.resetTypes().map(_.emit).toBuffer)
    }

    for ((name, objs) <- files) {

      val fileText = if (name != "three") {
        val currentImports = imports(name)
        headers + objs.map(_.prependChildren(currentImports).emit()).mkString("\n")
      } else {
        val currentImports = imports(name).emit()
        val globalDefs = globalDefintions.emit()
        headers + globalDefs + currentImports + objs.map(_.emit()).mkString("\n")
      }
      writeFileText(s"$outDir/$name.scala", fileText)
    }
  }

  emit(
    "out/src/main/scala",
    transform(parseRootDirectory(Seq(), new File("data/src")))
  )

}

//  EXPORTING TODOs:
//  - Add Missing type definitions
//  - Sanitize method names
//  - Apply default type arguments
