package tsparse

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set

class Lines(var leaf: Seq[String], children: Buffer[Lines] = Buffer()) {
  // Todo: might be more efficient to write this into a stringBuilder
  // or directly into a file
  def emit(indent: Int = 0): String = {
    val tabs = "\t" * indent
    val s1 = leaf.map(tabs + _).mkString("\n")
    val sep = if (children.length > 0) "\n" else ""
    val s2 = children.map(_.emit(indent + 1)).mkString("\n")
    s1 + sep + s2
  }

  def pad(): Lines = {
    leaf = Seq("") ++ leaf ++ Seq("\n")
    this
  }

  def prependChildren(newChild: Lines) = {
    children.prepend(newChild)
    this
  }

}

object Lines {
  def apply(leaves: String*) = new Lines(leaves, Buffer())
}

object Emitter {

  class TypeContext {
    case class TypeKey(args: Seq[Argument], keys: Seq[Key])

    var defaultTypeArgs = Map[String, Seq[DataType]]()

    var genIndex = 0
    val types = Map[TypeKey, String]()

    // TODO: better naming from method context, etc.
    def newName() = {
      genIndex += 1
      s"objectType$genIndex"
    }

    def typeName(members: Seq[Argument], keys: Seq[Key]) = {
      val key = TypeKey(members, keys)
      types.getOrElseUpdate(key, newName())
    }

    def resetTypes(): Iterable[SJSTopLevel] = {
      val result = types.map { case (TypeKey(members, keys), name) =>
        // TODO: HACK.
        // Right now this is safe because the value members are never static, so they never have to go in
        // a companion object.
        implicit val ____ = new TransformContext("")
        val mems: Seq[InterfaceMember] =
          members.map(arg => ValueMember(arg.name, arg.dataType, arg.optional, false, false)) ++
            keys.map(key => KeyMember(key))
        new NativeTrait(name, None, None, mems.map(_.transform))
      }
      types.clear()
      result
    }

  }

  def matchBaseName(name: String): String = {
    name match {
      case "void" => "Unit"

      case "true"    => "Boolean"
      case "false"   => "Boolean"
      case "boolean" => "Boolean"

      case "null"   => "Null"
      case "this"   => "this.type"
      case "any"    => "js.Any"
      case "object" => "js.Object"

      case "number" => "Double"
      case "string" => "String"

      case "Promise" => "js.Promise"
      case "RegExp"  => "js.RegExp"
      case "Map"     => "js.Map"
      case _         => name
    }
  }

  import Helpers._

  def emitType(t: DataType)(implicit context: TypeContext): String =
    t match {
      case Base(name) => {
        context.defaultTypeArgs.get(name) match {
          case None        => matchBaseName(name)
          case Some(value) => emitType(Parameterized(name, value))
        }
      }
      case StringType(value) => value.replace('\'', '"')
      case ArrayType(member) => s"js.Array[${emitType(member)}]"
      case UnionType(members) => {
        reifyOptional(t, false) match {
          case UnionType(members) => members.map(emitType).mkString(" | ")
          case other              => emitType(other)
        }
      }
      case Parameterized(name, parameters) =>
        s"${matchBaseName(name)}[${parameters.map(emitType).mkString(",")}]"
      case ObjectType(members, keys) => context.typeName(members, keys)
      case ArrowType(typeParameters, parameters, ret) => {
        val lng = parameters.args.length
        val typs = parameters.args.map(a => a.dataType) :+ ret
        s"js.Function$lng[${typs.map(emitType).mkString(",")}]"
      }
      case IntersectionType(_) => "js.Any"
      case TupleType(members) => {
        if (members.length > 0 && members.count(t => t == members(0)) == members.length) {
          s"js.Array[${emitType(members(0))}]"
        } else {
          s"js.Array[js.Any]"
        }
      }
    }

}

import Emitter._

sealed trait SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines
  var name: String
  var jsName: String
}

object SJSTopLevel {
  def empty: SJSTopLevel = new SJSTopLevel {
    var name = ""
    var jsName = ""
    def emit(implicit ctx: TypeContext): Lines = new Lines(Seq(), Buffer())
  }
}

object Helpers {
  def reifyOptional(typ: DataType, optional: Boolean): DataType = {
    typ match {
      case UnionType(members) if members contains Base("undefined") => {
        val filtered = members.filter(_ != Base("undefined"))
        val subType = if (filtered.length > 1) UnionType(filtered) else filtered(0)
        Parameterized("js.UndefOr", Seq(subType))
      }
      case _ => if (optional) Parameterized("js.UndefOr", Seq(typ)) else typ
    }
  }

  def sanitize(name: String) = {
    name match {
      case "val" | "type" | "object" | "new" => s"`$name`"
      case _                                 => name
    }
  }

  // Sanitize concrete string, number, and boolean types for use in js.native vars
  def sanitizeType(t: DataType) = {
    t match {
      case Base(name) => {
        name.toIntOption
          .map(_ => Base("number"))
          .getOrElse(name match {
            case "true"  => Base("boolean")
            case "false" => Base("boolean")
            case _       => Base(name)
          })
      }
      case StringType(value) => Base("string")
      case _                 => t
    }
  }

  def formatExtensions(extensions: Option[Seq[DataType]])(implicit ctx: TypeContext): String =
    extensions
      .map(exts => s"extends ${exts.map(emitType).mkString(" with ")}")
      .getOrElse("extends js.Object")

  def formatTypeArgs(args: Option[Seq[Generic]])(implicit ctx: TypeContext): String =
    args.map(a => s"[${a.map(_.emit).mkString(",")}]").getOrElse("")

  def formatArgList(originalArgs: ArgList)(implicit ctx: TypeContext): String = {
    def formatArgument(a: Argument): String = {
      val typ = emitType(reifyOptional(a.dataType, a.optional))
      s"${sanitize(a.name)}: $typ"
    }

    def formatVarArg(a: Argument): String = s"${sanitize(a.name)} : (${emitType(a.dataType)})*"

    val args = originalArgs.args.map(formatArgument)
    val argsAndVar = originalArgs.varArg match {
      case Some(v) => args :+ formatVarArg(v)
      case None    => args
    }
    s"(${argsAndVar.mkString(", ")})"
  }
}

import Helpers._

trait TraitDeclaration extends SJSTopLevel

class NativeTrait(
    var name: String,
    typeArgs: Option[Seq[Generic]],
    extensions: Option[Seq[DataType]],
    members: Seq[SJSTopLevel]
) extends TraitDeclaration {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    val typeParameters = formatTypeArgs(typeArgs)
    val extendsClause = formatExtensions(extensions)
    val colon = if (members.size > 0) ":" else ""
    new Lines(
      Seq(
        "",
        "@js.native",
        // s"@JSGlobal(\"$jsName\")",
        s"sealed trait $name$typeParameters $extendsClause$colon"
      ),
      members.map(_.emit).toBuffer
    )
  }
}

class ScalaTrait(var name: String, extensions: Option[Seq[DataType]], members: Seq[SJSTopLevel])
    extends TraitDeclaration {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    val extendsClause = formatExtensions(extensions)
    new Lines(Seq(s"trait $name $extendsClause:"), members.map(_.emit).toBuffer)
  }
}

class NativeClass(
    var name: String,
    typeArgs: Option[Seq[Generic]],
    constructors: Seq[Constructor],
    members: Buffer[SJSTopLevel],
    extensions: Option[Seq[DataType]]
) extends SJSTopLevel {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    val typPars = formatTypeArgs(typeArgs)

    val ctors = constructors
      .filter(ps => ps.parameters.args.length > 0 || ps.parameters.varArg.isDefined)
      .map(ps => Lines(s"def this${formatArgList(ps.parameters)} = this()"))
      .toBuffer

    val colon = if (members.size + ctors.size > 0) ":" else ""

    new Lines(
      Seq(
        "",
        "@js.native",
        s"@JSGlobal(\"$jsName\")",
        s"class $name$typPars ${formatExtensions(extensions)}$colon"
      ),
      ctors ++ members.map(_.emit)
    )
  }
}
class TypeAlias(var name: String, dataType: DataType) extends SJSTopLevel {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    Lines("", s"type $name = ${emitType(dataType)}")
  }
}
class NativeConstant(var name: String, dataType: DataType) extends SJSTopLevel {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    Lines(s"val ${sanitize(name)}: ${emitType(sanitizeType(dataType))} = js.native;")
  }
}
class NativeValue(var name: String, dataType: DataType) extends SJSTopLevel {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    Lines(s"var ${sanitize(name)}: ${emitType(sanitizeType(dataType))} = js.native;")
  }
}

case class Generic(typ: String, superType: Option[DataType]) {
  def emit(implicit ctx: TypeContext): String = {
    superType.map(default => s"$typ <: ${emitType(default)}").getOrElse(typ)
  }
}

class NativeFunction(
    var name: String,
    typeArgs: Option[Seq[Generic]],
    args: Option[ArgList],
    ret: DataType,
    bracket: Boolean = false
) extends SJSTopLevel {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines = {
    val cleanName = sanitize(name)
    val typPars = formatTypeArgs(typeArgs)
    val apars = args.map(formatArgList).getOrElse("")
    val annots = if (bracket) Seq("@JSBracketAccess") else Seq()
    val defn = s"def $cleanName$typPars$apars: ${emitType(ret)} = js.native"
    new Lines(annots ++ Seq(defn))
  }
}

class NativeObject(
    var name: String,
    var jsName: String,
    members: Buffer[SJSTopLevel] = Buffer()
) extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines = {
    val colon = if (members.size > 0) ":" else ""
    new Lines(
      Seq("", "@js.native", s"@JSGlobal(\"$jsName\")", s"object $name extends js.Object$colon"),
      members.map(_.emit)
    )
  }
}

class CompanionObject(var name: String, val members: Buffer[SJSTopLevel] = Buffer())
    extends SJSTopLevel {
  var jsName: String = ""
  def emit(implicit ctx: TypeContext): Lines =
    new Lines(Seq("", s"object $name:"), members.map(_.emit))
}

class Module(val name: String, val members: Buffer[SJSTopLevel] = Buffer()) {
  val subModules = Map[String, Module]()

  def moduleNames(root: Option[String] = None): Seq[String] =
    subModules.flatMap {
      case (name, sub) => {
        val nextPrefix = root.map(r => s"$r.$name").getOrElse(name)
        Seq(nextPrefix) ++ sub.moduleNames(Some(nextPrefix))
      }
    }.toSeq

  def flattenModules(root: Option[String] = None): Seq[Module] = {
    val currentName = root.map(r => s"$r.$name").getOrElse(name)
    val currentMod = new Module(currentName, members)
    Seq(currentMod) ++ subModules.values.flatMap(sub => sub.flattenModules(Some(currentName)))
  }

}

// Transformations we need to make everything work:
//
//  - Interface/structural types need to be reified into named trait declarations and referenced
//  - static methods and values need to be placed into a companion object
//  - methods with the same signature as an identically-named method in a trait they are
//    extending should be removed.
//  - parameterized types with a default type argument will have that argument detected
//    and then substituted at all invocation sites of this type.
//  - methods with names reserved in scala (`clone`, `val`, `then`) should be converted
//    to scala names (`cloned`, `value`, `andThen`) if there is no conflict
//  - type parameters on arrow types are lifted to the nearest scope that can provide a type parameter in Scala,
//    i.e.
//         foo: <B extends A>(arg: B): A
//    becomes
//        def foo[B <: A](arg: B): A
//
// // - Bonus: explicit subtyping should follow the structure, i.e. instead of
//
//          trait foo extends js.Object { val x: Double }
//          trait bar extends js.Object { val x: Double; val y: Double; }
//
//      we have:
//
//          trait foo extends js.Object { val x: Double = js.native }
//          trait bar extends foo { val y: Double = js.native }
//
//      so that things will work out the way they do in TypeScript, and we can
//      pass objects of type bar to methods that expect foo, which otherwise is
//      not possible.
//
// // - Bonus: extension methods returning the object type should be generated for any method that:
//    - returns unit OR
//    - prefixed with `set()` or `add()`
//    to allow builder-style construction interfaces.

// structure:
// - each folder will become a module, which is an object that looks like:
//
//  @js.native
//  @JSGlobal("THREE")
//  object $name extends js.Object {
//    import $dependency.*
//  }
//
// It will import dependencies from all the modules it needs to.
// Note that all of the JSGlobal("THREE") annotations will make it so that
// these access the same JS object at runtime. By default we will have
// everything import everything else.

// class FacadeModule(name: String, members: Buffer[SJSTopLevel])
