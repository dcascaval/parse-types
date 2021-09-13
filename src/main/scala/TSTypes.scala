package tsparse

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.annotation.meta.companionObject

// all of the different possible types
sealed trait DataType
case class Base(name: String) extends DataType // number, undefined, void, etc.
case class StringType(value: String) extends DataType // 'foo'
case class Parameterized(name: String, parameters: Seq[DataType]) extends DataType // ArrayLike<number>
case class ArrayType(member: DataType) extends DataType // number[]
case class ArrowType(typeParameters: Option[Seq[TypeParameterDecl]], parameters: ArgList, ret: DataType)
    extends DataType
case class TupleType(members: Seq[DataType]) extends DataType // [number, number, number]
// TODO: remove undefined from this set if it's optional on an arg/parameter
case class UnionType(members: Seq[DataType]) extends DataType // number | undefined
case class IntersectionType(members: Seq[DataType]) extends DataType
case class ObjectType(members: Seq[Argument], keys: Seq[Key]) extends DataType // { a: T; b: Q }

case class Argument(name: String, dataType: DataType, optional: Boolean)
case class Key(name: String, dataType: DataType, returnType: DataType)

object Argument {
  def fromRaw(name: String, data: DataType, optional: Boolean): Argument = {
    // When we have an optional argument that includes undefined in the type
    // member, we can safely remove undefined from the union type.
    if (optional) {
      data match {
        case UnionType(members) => {
          val withoutUndefined = members.filter(_ != Base("undefined"))
          withoutUndefined match {
            case Seq(single) => return Argument(name, single, optional)
            case rest        => return Argument(name, UnionType(withoutUndefined), optional)
          }
        }
        case _ => ()
      }
    }
    Argument(name, data, optional)
  }
}

// foo(a: bar, b: number, ...args[])
case class ArgList(args: Seq[Argument], varArg: Option[Argument])

sealed trait GetSetState
case object Getter extends GetSetState
case object Setter extends GetSetState

sealed trait Member
sealed trait InterfaceMember extends Member

// [readonly] foo: A;
case class ValueMember(
    name: String,
    dataType: DataType,
    optional: Boolean,
    readOnly: Boolean,
    static: Boolean
) extends InterfaceMember

case class KeyMember(key: Key) extends InterfaceMember

// setFoo(foo: A, bar?: B);
case class FnMember(
    name: String,
    typeParameters: Option[Seq[TypeParameterDecl]],
    parameters: ArgList,
    returnType: Option[DataType],
    getSet: Option[GetSetState],
    static: Boolean // Mutually exclusive with get/set
) extends InterfaceMember

case class Constructor(
    parameters: ArgList
) extends Member

// In scala there isn't such a thing as a default type parameter, so we
// will instead remember the default and replace it at all invocation sites.
case class TypeParameterDecl(name: String, extension: Option[DataType], default: Option[DataType])

class TransformContext(module: String) {
  var currentModule = new NativeObject(
    module.toLowerCase(),
    "THREE"
  )
  val companions = Map[String, CompanionObject]()
  def mergeToCompanion(name: String, member: SJSTopLevel) = {
    companions
      .getOrElseUpdate(name, new CompanionObject(name))
      .members += member
  }

  val typeMapping = Map[String, DataType]()

  def withDefaults(t: String) = {}

  def addGlobalTypeDefault(t: TypeParameterDecl) = {
    t.default.map(default => typeMapping += ((t.name, default)))
  }

  // TODO: update to support shadowing correctly
  def withLocalTypeDefault[A](t: TypeParameterDecl, f: => A): A = {
    t.default match {
      case Some(default) => {
        typeMapping += ((t.name, default))
        val result = f
        typeMapping -= t.name
        result
      }
      case None => f
    }

  }
}

sealed trait TopLevelStatement {
  def transform(implicit ctx: TransformContext): SJSTopLevel
}

// export const foo: A;
case class Constant(
    name: String,
    dataType: DataType
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) =
    new NativeConstant(name, dataType)
}

sealed trait EnumMember
case class StringMem(value: String) extends EnumMember
case class ValueMem(name: String, value: Int) extends EnumMember
case class BasicMember(name: String) extends EnumMember

// Ex:
// export enum Foo { A = 0, BAR, }
// export enum Baz { 'foo', 'bar' }
case class TopLevelEnum(
    name: String,
    options: Seq[EnumMember]
) extends TopLevelStatement {

  def transform(implicit ctx: TransformContext): SJSTopLevel = {
    val allStrings = options.flatMap[StringType](mem =>
      mem match {
        case StringMem(s) => Some(StringType(s))
        case _            => None
      }
    )
    if (allStrings.length == options.length) {
      new TypeAlias(name, UnionType(allStrings))
    } else {
      val members = options
        .flatMap[String](mem =>
          mem match {
            case BasicMember(name) => Some(name)
            case StringMem(value)  => None
            case ValueMem(name, _) => Some(name)
          }
        )
        .map[SJSTopLevel](memName => new NativeConstant(memName, Base(name)))
        .toBuffer

      new NativeObject(name, s"THREE.$name", members)
    }
  }
}

// export class Foo extends Bar {
//   constructor(params?: ParamType);  // constructor
//   foo: A;                           // member
//   setFoo(foo: A): ReturnType;       // function
// }
class Class(
    name: String,
    typeParameters: Option[Seq[TypeParameterDecl]],
    values: Seq[ValueMember],
    functions: Seq[FnMember],
    constructors: Seq[Constructor],
    extensions: Option[Seq[DataType]],
    implements: Option[DataType] // class Foo implements IFoo {}
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = ???
}

// export type Foo = [number, number] | string;
case class TopLevelType(name: String, dataType: DataType) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = ???
}
//
// export interface Foo<T = any> {
//   bar: number;
//   baz: T;
// }
//
case class Interface(
    name: String,
    parameters: Option[Seq[TypeParameterDecl]],
    members: Seq[InterfaceMember],
    extensions: Option[Seq[DataType]]
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = ???
}

case class Namespace(
    name: String,
    members: Seq[TopLevelStatement]
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = ???
}

case class Function(
    value: FnMember
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = {
    val FnMember(name, t, args, ret, getSet, static) = value
    val typeArgs = t.map(ts => ts.map(t => Generic(t.name, t.extension)))

    val returnType = ret.getOrElse(Base("Unit"))

    val fn = getSet match {
      case Some(Getter) => // def $name: ret
        new NativeFunction(name, typeArgs, None, returnType)
      case _ => // def $name<$T>($args) : ret
        new NativeFunction(name, typeArgs, Some(args), returnType)
    }

    if (static) {
      ctx.companions(ctx.currentModule.name).members += fn
      SJSTopLevel.empty
    } else {
      fn
    }
  }
}
