package tsparse

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import javax.xml.crypto.Data

class Lines(leaf: Seq[String], children: Buffer[Lines] = Buffer()) {
  // Todo: might be more efficient to write this into a stringBuilder
  // or directly into a file
  def emit(indent: Int = 0): String = {
    val tabs = "\t" * indent
    val s1 = leaf.map(tabs + _).mkString("\n")
    val s2 = children.map(_.emit(indent + 1)).mkString("\n")
    s1 + s2
  }
}

object Lines {
  def apply(leaves: String*) = new Lines(leaves, Buffer())
}

object Emitter {

  class TypeContext {
    case class TypeKey(args: Seq[Argument], keys: Seq[Key])

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
  }

  def matchBaseName(name: String): String = {
    name match {
      case "void"    => "Unit"
      case "this"    => "this.type"
      case "any"     => "js.Any"
      case "object"  => "js.Object"
      case "number"  => "Double"
      case "string"  => "String"
      case "boolean" => "Boolean"
      case _         => name
    }
  }

  def emitType(t: DataType)(implicit context: TypeContext): String =
    t match {
      case Base(name)        => matchBaseName(name)
      case StringType(value) => value
      case ArrayType(member) => s"js.Array[${emitType(member)}]"
      case UnionType(members) =>
        if (members.contains(Base("undefined"))) {
          val filteredMembers = members.filter(_ != Base("undefined"))
          s"js.UndefOr[${filteredMembers.map(emitType).mkString}]"
        } else {
          members.map(emitType).mkString(" | ")
        }
      case Parameterized(name, parameters) => s"$name[${parameters.map(emitType).mkString(",")}]"
      case ObjectType(members, keys)       => context.typeName(members, keys)
      case ArrowType(typeParameters, parameters, ret) => {
        val lng = parameters.args.length
        s"js.Function$lng[${parameters.args.map(a => emitType(a.dataType)).mkString(",")},${emitType(ret)}]"
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
}

object SJSTopLevel {
  def empty: SJSTopLevel = new SJSTopLevel {
    def emit(implicit ctx: TypeContext): Lines = new Lines(Seq(), Buffer())
  }
}

object Helpers {
  def formatExtensions(extensions: Option[Seq[DataType]])(implicit ctx: TypeContext): String =
    extensions
      .map(exts => s"extends ${exts.map(emitType).mkString(" with ")}")
      .getOrElse("extends js.Object")

  def formatTypeArgs(args: Option[Seq[Generic]])(implicit ctx: TypeContext): String =
    args.map(a => s"[${a.map(_.emit).mkString(",")}]").getOrElse("")

  def formatArgList(originalArgs: ArgList)(implicit ctx: TypeContext): String = {
    def formatArgument(a: Argument): String = {
      val subType = emitType((a.dataType))
      val typ = if (a.optional) { s"js.UndefOr[$subType] = js.undefined" }
      else subType
      s"${a.name}: $typ"
    }

    def formatVarArg(a: Argument): String = s"${a.name} : (${emitType(a.dataType)})*"

    val args = originalArgs.args.map(formatArgument)
    val argsAndVar = originalArgs.varArg match {
      case Some(v) => args +: formatVarArg(v)
      case None    => args
    }
    s"(${argsAndVar.mkString(", ")})"
  }
}

import Helpers._

trait TraitDeclaration extends SJSTopLevel

class NativeTrait(
    name: String,
    typeArgs: Option[Seq[Generic]],
    extensions: Option[Seq[DataType]],
    members: Seq[SJSTopLevel]
) extends TraitDeclaration {
  def emit(implicit ctx: TypeContext): Lines = {
    val typeParameters = formatTypeArgs(typeArgs)
    val extendsClause = formatExtensions(extensions)
    new Lines(
      Seq("@js.native" + s"trait $name$typeParameters $extendsClause:"),
      members.map(_.emit).toBuffer
    )
  }
}

class ScalaTrait(name: String, extensions: Option[Seq[DataType]], members: Seq[SJSTopLevel])
    extends TraitDeclaration {
  def emit(implicit ctx: TypeContext): Lines = {
    val extendsClause = formatExtensions(extensions)
    new Lines(Seq(s"trait $name $extendsClause:"), members.map(_.emit).toBuffer)
  }
}

class NativeClass(
    name: String,
    typeArgs: Option[Seq[Generic]],
    constructors: Seq[Constructor],
    members: Buffer[SJSTopLevel],
    extensions: Option[Seq[DataType]]
) extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines = {
    val typPars = formatTypeArgs(typeArgs)

    val ctors = constructors.map(ps => formatArgList(ps.parameters))

    new Lines(
      Seq("@js.native", s"class $name$typPars ${formatExtensions(extensions)}:"),
      members.map(_.emit)
    )
  }
}
class TypeAlias(name: String, dataType: DataType) extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines = {
    Lines(s"type $name = ${emitType(dataType)}")
  }
}
class NativeConstant(name: String, dataType: DataType) extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines = {
    Lines(s"val $name: ${emitType(dataType)} = js.native;")
  }
}
class NativeValue(name: String, dataType: DataType) extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines = {
    Lines(s"var $name: ${emitType(dataType)} = js.native;")
  }
}

case class Generic(typ: String, superType: Option[DataType]) {
  def emit(implicit ctx: TypeContext): String = {
    superType.map(default => s"$typ <: ${emitType(default)}}").getOrElse(typ)
  }
}

class NativeFunction(
    name: String,
    typeArgs: Option[Seq[Generic]],
    args: Option[ArgList],
    ret: DataType,
    bracket: Boolean = false
) extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines = {
    val typPars = formatTypeArgs(typeArgs)
    val apars = args.map(formatArgList).getOrElse("")
    val annots = if (bracket) Seq("@JSBracketAccess") else Seq()
    val defn = s"def $name$typPars$apars: ${emitType(ret)}) = js.native"
    new Lines(annots ++ Seq(defn))
  }
}

class NativeObject(val name: String, jsName: String, val members: Buffer[SJSTopLevel] = Buffer())
    extends SJSTopLevel {
  val imports = Buffer[String]()

  def emit(implicit ctx: TypeContext): Lines = {
    new Lines(
      Seq("@js.native", s"JSGlobal(\"$jsName\")", s"object $name extends js.Object:"),
      members.map(_.emit)
    )
  }
}

class CompanionObject(val name: String, val members: Buffer[SJSTopLevel] = Buffer())
    extends SJSTopLevel {
  def emit(implicit ctx: TypeContext): Lines =
    new Lines(Seq(s"object $name:"), members.map(_.emit))
}

// Special case the very common scala-js-defined parameter traits
class ParameterClass(name: String, extensions: Seq[DataType], variables: Seq[(String, DataType)]) {
  def emit(implicit context: TypeContext): Lines = {
    val exts = extensions match {
      case Seq()       => "js.Object"
      case Seq(single) => s"$single"
      case _           => extensions.map(emitType).mkString(" with ")
    }
    val header = s"trait $name extends $exts:"
    val members = variables.map { case (name, typ) =>
      Lines(s"var $name: ${emitType(typ)} = js.undefined")
    }.toBuffer
    new Lines(Seq(header), members)
  }
}

// type => type

// class TypeDeclaration(name: String, t: DataType) extends FacadeObject {
//   def emit(implicit ctx: TypeContext) = s"type $name = ${emitType(t)}"
// }

// function => top-level function with
//
//        @js.native
//        @JSGlobal(THREE.foo)
//        def foo() = ...

// class FunctionDeclaration(name: String, args: Seq[Argument], ret: DataType) extends FacadeObject {
//   def emit(implicit ctx: TypeContext) = {
//     val argList = args
//       .map(a => {
//         val typeName = emitType(a.dataType)
//         val t = if (a.optional) s"js.UndefOr[$typeName]" else typeName
//         s"${a.name}: $t"
//       })
//       .mkString(", ")
//     Seq("@js.native", s"@JSGlobal(\"THREE.$name\")", s"def $name($argList) = js.native")
//       .mkString("\n")
//   }
// }

// value => value
// (also global, as functions)

// class ValueDeclaration(name: String, typ: DataType) extends FacadeObject {
//   def emit(implicit ctx: TypeContext) = {
//     Seq("@js.native", s"@JSGlobal(\"THREE.$name\")", s"const $name : ${emitType(typ)} = js.native")
//       .mkString("\n")
//   }
// }

// enum => @js.native sealed trait,
//         with fields in companion object and @JSBracketAcess apply().
//         their actual contained values are irrelevant.

// class EnumDeclaration(name: String, members: Seq[EnumMember]) extends FacadeObject {
//   def emit(implicit ctx: TypeContext): String = {
//     val allStrings = members.flatMap[StringMem](mem =>
//       mem match {
//         case s: StringMem => Some(s)
//         case _            => None
//       }
//     )
//     if (allStrings.length == members.length) {
//       s"type $name = ${allStrings.map(_.value).mkString(" | ")}"
//     } else {
//       val memberNames = members
//         .flatMap[String](mem =>
//           mem match {
//             case BasicMember(name) => Some(name)
//             case StringMem(value)  => None
//             case ValueMem(name, _) => Some(name)
//           }
//         )
//         .map(memName => s"\tvar $memName : $name = js.native")
//       (Seq(
//         "@js.native",
//         s"@JSGlobal(\"THREE.${name}\")",
//         s"object $name extends js.Object:"
//       ) ++ memberNames).mkString("\n")
//     }
//   }
// }

// interface => @js.native trait

// class InterfaceDeclaration(name: String, members: Seq[InterfaceMember]) extends FacadeObject {
//   def emit(implicit ctx: TypeContext): String = {
//     ???
//   }
// }

// class => @js.native class
// - constructors: def this(args) = this()
// - static methods: companion object

// class ClassDeclaration(name: String)

// namespace => static scala object

// class NamespaceDeclaration(name: String, members: Seq[TopLevelStatement]) extends FacadeObject {
//   def emit(implicit ctx: TypeContext): String = {
//     ???
//   }
// }

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