package tsparse

import scala.collection.mutable.ArrayBuffer

// all of the different possible types
sealed trait DataType
case class Base(name: String) extends DataType // number, undefined, void, etc.
case class StringType(value: String) extends DataType // 'foo'
case class Parameterized(name: String, parameters: Seq[DataType]) extends DataType // ArrayLike<number>
case class Union(members: Set[DataType]) extends DataType // number | undefined
case class ArrayOf(member: DataType) extends DataType // number[]
case class Arrow(parameters: Seq[Argument], ret: DataType) extends DataType
case class ConstArray(members: Seq[DataType]) extends DataType // [number, number, number]

case class Argument(name: String, dataType: DataType, optional: Boolean)
// [readonly] foo: A;
case class Member(name: String, dataType: DataType, optional: Boolean = false, readOnly: Boolean = false)
// static foo: A;
case class StaticMember(name: String, datatype: DataType) // Goes in companion object
// setFoo(foo: A, bar?: B);
case class FnMember(name: String, parameters: Seq[Member])

// In scala there isn't such a thing as a default type parameter, so we
// will instead remember the default and replace it at all invocation sites.
case class TypeParameterDecl(name: String, default: Option[DataType])

sealed trait TopLevelStatement

// export const foo: A;
class TopLevelConstant(
    name: String,
    dataType: String
) extends TopLevelStatement

// export enum Foo {
//  A = 0,
//  B = "x",
//  C
// }
class TopLevelEnum(
    name: String,
    options: Seq[(String, Option[String])]
) extends TopLevelStatement

// export class Foo extends Bar {
//   constructor(params?: ParamType);  // constructor
//   foo: A;                           // member
//   setFoo(foo: A): ReturnType;       // function
// }
class BasicClass(
    name: String,
    typeParameters: Seq[String],
    members: Seq[Member],
    functions: Seq[FnMember],
    constructors: Seq[FnMember],
    extensions: Seq[String],
    implements: Seq[String] // class Foo implements IFoo {}
) extends TopLevelStatement

// export type Foo = [number, number] | string;
class TopLevelType(name: String, dataType: DataType) extends TopLevelStatement

//
// export interface Foo<T = any> {
//   bar: number;
//   baz: T;
// }
//
class Interface(
    name: String,
    parameters: Seq[TypeParameterDecl],
    members: Seq[Member],
    extensions: Seq[String]
) extends TopLevelStatement
