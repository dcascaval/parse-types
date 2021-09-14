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
object Preprocessors {

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
    ),

    // This is an overload that fails due to type erasure, and isn't even useful
    // since we have no real representation of js tuple types, since JS tuples are arrays.
    matrixToArray("Vector2"),
    matrixToArray("Vector3"),
    matrixToArray("Vector4"),
    matrixToArray("Matrix3"),
    matrixToArray("Matrix4"),
    Preprocessor(
      file = "WebXR.d.ts",
      oldText = """export interface XRReferenceSpace extends EventTarget {
      |    getOffsetReferenceSpace(originOffset: XRRigidTransform): XRReferenceSpace;
      |}
      |export interface XRHitTestOptionsInit {
      |    space: EventTarget;
      |    offsetRay?: XRRay | undefined;
      |}
      |
      |export interface XRTransientInputHitTestOptionsInit {
      |    profile: string;
      |    offsetRay?: XRRay | undefined;
      |}""".stripMargin,
      newText = ""
    )
  )

  def matrixToArray(typeName: String) =
    Preprocessor(
      file = s"src/math/$typeName.d.ts",
      oldText = s"toArray(array?: ${typeName}Tuple, offset?: 0): ${typeName}Tuple;",
      newText = ""
    )
}

object Main extends App {
  type FileResult = (Seq[String], Seq[TopLevelStatement])
  val preprocessors = Preprocessors.preprocessors

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

  def transform(results: Seq[FileResult]): (Module, TransformContext) = {
    implicit val transformContext = new TransformContext("")

    val rootModule = new Module("three")

    def getOrCreateModule(path: Seq[String], root: Module = rootModule): Module = {
      def newModule(name: String) = new Module(name)
      path.foldLeft(root) { case (mod, name) =>
        mod.subModules.getOrElseUpdate(name, newModule(name))
      }
    }

    for (result <- results) {
      val (path, tree) = result
      val module = getOrCreateModule(path)
      module.members ++= tree.map(_.transform)
      module.members ++= transformContext.resetCompanions()
    }

    (rootModule, transformContext)
  }

  val headers = Lines(
    "\n",
    "import scala.scalajs.js",
    "import js.annotation.*",
    "import org.scalajs.dom.*",
    "import org.scalajs.dom.raw.{HTMLMediaElement, HTMLVideoElement, HTMLCanvasElement, HTMLImageElement}",
    "import org.scalajs.dom.raw.{WebGLShader, WebGLFramebuffer}",
    "import org.scalajs.dom.experimental.gamepad.*",
    "import scalajs.js.typedarray.*",
    ""
  ).emit()

  // Usually imports from TS Stdlib
  val globalDefintions = Lines(
    "type ArrayLike[T] = js.native",
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

  // What's the right architecture for this?
  // Every module file should have:
  // - its own sub-package
  // - all of its definitions prefixed with @JS.Global("THREE.$name")
  def emit(outDir: String, inputs: (Module, TransformContext)) = {
    val (root, transformContext) = inputs
    val allModules = root.flattenModules()
    val moduleNames = allModules.map(_.name)

    def imports(currentModule: String) =
      new Lines(
        moduleNames
          .filter(_ != currentModule)
          .map(mod => s"import typings.three.$mod.*")
      ).pad()

    def globals(currentModule: String) =
      if (currentModule == "three") globalDefintions else ""

    implicit val ctx = new Emitter.TypeContext()
    ctx.defaultTypeArgs = transformContext.typeMapping

    for (module <- allModules) {
      val packageName = s"package typings.three.${module.name}"
      val fileName = module.name.replace('.', '/')

      val currentImports = imports(module.name).emit()
      val currentGlobals = globals(module.name)

      val firstMembers = module.members
        .map(member => { member.jsName = s"THREE.${member.name}"; member })
        .map(_.emit.emit())
      val interfaceTypes = ctx.resetTypes().map(_.emit.emit())
      val members = firstMembers ++ interfaceTypes

      val fileText =
        packageName + headers + currentImports + currentGlobals + members.mkString("\n")

      writeFileText(s"$outDir/$fileName.scala", fileText)
    }
  }

  emit(
    "out/src/main/scala",
    transform(parseRootDirectory(Seq(), new File("data/src")))
  )

}

//  EXPORTING TODOs:
//  - lifting generic arrowTypes to nearest scope (probably not going to do this)
//  - importing scalajs.dom.* brings in a name conflict
//  - duplicate definitions in WebXR
//  - Namespaces conflict with companion objects -> check if the companion exists before creating it
//    (or just merge it)
