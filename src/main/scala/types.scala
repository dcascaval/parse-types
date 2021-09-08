package tsparse

import scala.collection.mutable.ArrayBuffer

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

// foo(a: bar, b: number, ...args: any[])
case class ArgList(args: Seq[Argument], varArg: Option[DataType])

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
    readOnly: Boolean
) extends InterfaceMember

// static foo: A;
case class StaticMember(name: String, datatype: DataType, readOnly: Boolean)
    extends Member // Goes in companion object

// setFoo(foo: A, bar?: B);
case class FnMember(
    name: String,
    typeParameters: Option[Seq[TypeParameterDecl]],
    parameters: ArgList,
    returnType: Option[DataType],
    getSet: Option[GetSetState]
) extends InterfaceMember

case class Constructor(
    parameters: ArgList
) extends Member

// In scala there isn't such a thing as a default type parameter, so we
// will instead remember the default and replace it at all invocation sites.
case class TypeParameterDecl(name: String, extension: Option[DataType], default: Option[DataType])

sealed trait TopLevelStatement

// export const foo: A;
case class TopLevelConstant(
    name: String,
    dataType: DataType
) extends TopLevelStatement

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
) extends TopLevelStatement

// export class Foo extends Bar {
//   constructor(params?: ParamType);  // constructor
//   foo: A;                           // member
//   setFoo(foo: A): ReturnType;       // function
// }
class Class(
    name: String,
    typeParameters: Option[Seq[TypeParameterDecl]],
    values: Seq[ValueMember],
    statics: Seq[StaticMember],
    functions: Seq[FnMember],
    constructors: Seq[Constructor],
    extensions: Option[Seq[DataType]],
    implements: Option[DataType] // class Foo implements IFoo {}
) extends TopLevelStatement

// export type Foo = [number, number] | string;
case class TopLevelType(name: String, dataType: DataType) extends TopLevelStatement

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
) extends TopLevelStatement
