package tsparse

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set

object Emitter {

  def emitDecl(t: TypeParameterDecl): String = ???

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

sealed trait FacadeObject {
  def emit(implicit ctx: TypeContext): String
}

// Special case the very common scala-js-defined parameter traits
class ParameterClass(name: String, extensions: Seq[DataType], variables: Seq[(String, DataType)])
    extends FacadeObject {
  def emit(implicit context: TypeContext): String = {
    val exts = extensions match {
      case Seq()       => "js.Object"
      case Seq(single) => s"$single"
      case _           => extensions.map(emitType).mkString(" with ")
    }
    val header = s"trait $name extends $exts:\n"
    val members = variables
      .map { case (name, typ) =>
        s"\tvar $name: ${emitType(typ)} = js.undefined"
      }
      .mkString("\n")
    header + members
  }
}

// type => type
class TypeDeclaration(name: String, t: DataType) extends FacadeObject {
  def emit(implicit ctx: TypeContext) = s"type $name = ${emitType(t)}"
}

// function => top-level function with
//
//        @js.native
//        @JSGlobal(THREE.foo)
//        def foo() = ...
class FunctionDeclaration(name: String, args: Seq[Argument], ret: DataType) extends FacadeObject {
  def emit(implicit ctx: TypeContext) = {
    val argList = args
      .map(a => {
        val typeName = emitType(a.dataType)
        val t = if (a.optional) s"js.UndefOr[$typeName]" else typeName
        s"${a.name}: $t"
      })
      .mkString(", ")
    Seq("@js.native", s"@JSGlobal(\"THREE.$name\")", s"def $name($argList) = js.native")
      .mkString("\n")
  }
}

// value => value
// (also global, as functions)
class ValueDeclaration(name: String, typ: DataType) extends FacadeObject {
  def emit(implicit ctx: TypeContext) = {
    Seq("@js.native", s"@JSGlobal(\"THREE.$name\")", s"const $name : ${emitType(typ)} = js.native")
      .mkString("\n")
  }
}

// enum => @js.native sealed trait,
//         with fields in companion object and @JSBracketAcess apply().
//         their actual contained values are irrelevant.
class EnumDeclaration(name: String, members: Seq[EnumMember]) extends FacadeObject {
  def emit(implicit ctx: TypeContext): String = {
    val allStrings = members.flatMap[StringMem](mem =>
      mem match {
        case s: StringMem => Some(s)
        case _            => None
      }
    )
    if (allStrings.length == members.length) {
      s"type $name = ${allStrings.map(_.value).mkString(" | ")}"
    } else {
      val memberNames = members
        .flatMap[String](mem =>
          mem match {
            case BasicMember(name) => Some(name)
            case StringMem(value)  => None
            case ValueMem(name, _) => Some(name)
          }
        )
        .map(memName => s"\tvar $memName : $name = js.native")
      (Seq(
        "@js.native",
        s"@JSGlobal(\"THREE.${name}\")",
        s"object $name extends js.Object:"
      ) ++ memberNames).mkString("\n")
    }
  }
}

// interface => @js.native trait
class InterfaceDeclaration(name: String, members: Seq[InterfaceMember]) extends FacadeObject {
  def emit(implicit ctx: TypeContext): String = {
    ???
  }
}

// class => @js.native class
// - constructors: def this(args) = this()
// - static methods: companion object
class ClassDeclaration(name: String)

// namespace => static scala object
class NamespaceDeclaration(name: String, members: Seq[TopLevelStatement]) extends FacadeObject {
  def emit(implicit ctx: TypeContext): String = {
    ???
  }
}

// Transformations we need to make everything work:
//
//  - Interface/structural types need to be reified into named trait declarations and referenced
//  - static methods and values need to be placed into a companion object
//  - methods with the same signature as an identically-named method in a trait they are
//    extending should be removed.
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

class FacadeModule(name: String, members: ArrayBuffer[FacadeObject])
