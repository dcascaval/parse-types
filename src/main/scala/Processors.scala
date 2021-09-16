package tsparse

// Use this when there's some typescript construct we don't know how to support in scala,
// so instead we string match.
// Currently we also make use of it when there's an edge case that appears literally once
// in the target library that we don't want to modify our code to support.
case class TextReplacement(file: String, oldText: String, newText: String) {
  def matches(targetFilePath: String) = targetFilePath.contains(file)
  def replace(targetText: String) = targetText.replace(oldText, newText)
  def apply(targetFilePath: String, targetText: String) =
    if (matches(targetFilePath)) replace(targetText) else targetText
}

trait TextProcessor {
  val processors: Seq[TextReplacement]
  def apply(filePath: String, text: String): String =
    processors.foldLeft(text)((current, processor) => processor(filePath, current))
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
object Preprocessors extends TextProcessor {

  val processors = Seq(
    // Not currently supporting conditional types, and there is only seemingly one instance in the definitions.
    TextReplacement(
      file = "src/renderers/webgl/WebGLCubeUVMaps.d.ts",
      oldText = "get<T>(texture: T): T extends Texture ? Texture : T;",
      newText = "get<T extends Texture>(texture: T): T;"
    ),
    // This completely breaks the parser, and we cannot allow empty idents.
    TextReplacement(
      file = "data/examples/jsm/libs/fflate.module.min.d.ts",
      oldText = "    (): void;",
      newText = ""
    ),
    // Instead of implementing our own type indexing system, just replace with this.
    TextReplacement(
      file = "data/examples/jsm/libs/fflate.module.min.d.ts",
      oldText = "GzipOptions['mtime'] | undefined",
      newText = "Date | string | number | undefined"
    ),
    // Typescript allows their keywords to be identifiers, for JS compat.
    TextReplacement(
      file = "data/examples/jsm/nodes/core/InputNode.d.ts",
      oldText = "readonly: boolean;",
      newText = "readonly readonly: boolean;"
    ),
    // It's either empty or a Uint8Array. However, we have no equivalent type in Scala.
    TextReplacement(
      file = "data/examples/jsm/exporters/MMDExporter.d.ts",
      oldText = "[] | Uint8Array",
      newText = "Uint8Array"
    ),
    // There is only a single instance of this in the entire definitions.
    TextReplacement(
      file = "data/examples/jsm/csm/Frustum.d.ts",
      oldText = "export default class",
      newText = "export class"
    ),

    // These are duplicate definitions that are strictly less expressive than the definitons
    // that come later.
    TextReplacement(
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
    ),

    // This property shadows the one already existing in js.Map[]
    TextReplacement(
      file = "WebXR.d.ts",
      oldText = """export interface XRHand extends Map<XRHandJoint, XRJointSpace> {
      |    readonly size: number;
      |}""".stripMargin,
      newText = "export interface XRHand extends Map<XRHandJoint, XRJointSpace> {}"
    ),
    // These are overloads that fail due to type erasure, and aren't even useful
    // since we have no real representation of js tuple types, since JS tuples are arrays.
    TextReplacement(
      file = s"src/math/",
      oldText = "toArray(array: ArrayLike<number>, offset?: number): ArrayLike<number>;",
      newText = ""
    ),
    TextReplacement(
      file = s"src/math/",
      oldText = "toArray(array?: ArrayLike<number>, offset?: number): ArrayLike<number>;",
      newText = ""
    ),
    TextReplacement(
      file = "src/math/Color.d.ts",
      oldText = "toArray(xyz: ArrayLike<number>, offset?: number): ArrayLike<number>;",
      newText = ""
    ),
    toArrayOverloads("Vector2"),
    toArrayOverloads("Vector3"),
    toArrayOverloads("Vector4"),
    toArrayOverloads("Matrix3"),
    toArrayOverloads("Matrix4"),
    // This interface allows type-unsafe access, and prevents type-safe overloads that each subtype has anyway.
    TextReplacement(
      file = "src/math/Vector2.d.ts",
      oldText = "set(...args: number[]): this;",
      newText = ""
    )
  )

  def toArrayOverloads(typeName: String) =
    TextReplacement(
      file = s"src/math/$typeName.d.ts",
      oldText = s"toArray(array?: ${typeName}Tuple, offset?: 0): ${typeName}Tuple;",
      newText = ""
    )

}

object Postprocessors extends TextProcessor {

  val processors = Seq(
    // Importing all of the other modules indiscriminately causes a name conflict.
    // Correct solution: Parse imports and exports to exactly import JS modules.
    TextReplacement(
      file = "renderers/webxr.scala",
      oldText = "trait XRInputSourceEvent extends Event:",
      newText = "trait XRInputSourceEvent extends org.scalajs.dom.Event:"
    ),
    // The type parameter `ObjectType` is present on the arrow type, but we don't want to make
    // a polymorphic scala value, so instead we shift it to a type variable on load.
    TextReplacement(
      file = "loaders.scala",
      oldText = "def load(url: String, onLoad: js.UndefOr[js.Function1[ObjectType,Unit]]",
      newText = "def load[ObjectType](url: String, onLoad: js.UndefOr[js.Function1[ObjectType,Unit]]"
    )
  )
}
