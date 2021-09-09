package tsparse

import fastparse._, JavaWhitespace._
import java.io.File
import scala.io.Source
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

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

object Main extends App {

  type FileResult = Seq[TopLevelStatement]

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

  // Not strictly optimized per se; but then we're not running this a lot.
  def parseFile(file: File): FileResult = {
    println(s"Parsing ${file.getPath()}")
    val string = getFileText(file)
    val parseResult = Parser.run(string)
    parseResult match {
      case f: Failure => {
        throw new Exception(f.trace().longMsg)
      }
      case Success(value, index) => {
        assert(index == string.length())
        value
      }
    }
  }

  def parseDirectory(file: File): Seq[FileResult] = {
    if (!file.isDirectory() && file.getName().endsWith(".d.ts")) {
      Seq(parseFile(file))
    } else {
      val files = file.listFiles()
      if (files == null) Seq() else files.toSeq.flatMap(parseDirectory)
    }
  }

  parseDirectory(new File("data/examples"))
  parseDirectory(new File("data/src"))
}
