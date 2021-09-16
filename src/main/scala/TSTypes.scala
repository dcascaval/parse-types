package tsparse

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

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

// foo(a: bar, b: number, ...args[])
case class ArgList(args: Seq[Argument], varArg: Option[Argument])

sealed trait GetSetState
case object Getter extends GetSetState
case object Setter extends GetSetState

sealed trait Member

sealed trait InterfaceMember extends Member {
  def transform(implicit ctx: TransformContext): SJSTopLevel
}

import Helpers._

// [readonly] foo: A;
case class ValueMember(
    name: String,
    dataType: DataType,
    optional: Boolean,
    readOnly: Boolean,
    static: Boolean
) extends InterfaceMember {
  def transform(implicit ctx: TransformContext) = {
    val t = reifyOptional(dataType, optional)

    val result = new NativeValue(name, t, mutable = !readOnly)

    if (static) {
      ctx.mergeToCompanion(ctx.currentModule, result)
      SJSTopLevel.empty
    } else result
  }
}

case class KeyMember(key: Key) extends InterfaceMember {
  def transform(implicit ctx: TransformContext): SJSTopLevel = {
    val Key(name, t, ret) = key
    new NativeFunction(
      "apply",
      None,
      Some(ArgList(Seq(Argument(name, t, false)), None)),
      ret,
      bracket = true
    )
  }
}

// setFoo(foo: A, bar?: B);
case class FnMember(
    name: String,
    typeParameters: Option[Seq[TypeParameterDecl]],
    parameters: ArgList,
    returnType: Option[DataType],
    getSet: Option[GetSetState],
    static: Boolean // Mutually exclusive with get/set
) extends InterfaceMember {
  def transform(implicit ctx: TransformContext): SJSTopLevel = Function(
    this
  ).transform
}

case class Constructor(
    parameters: ArgList
) extends Member

// In scala there isn't such a thing as a default type parameter, so we
// will instead remember the default and replace it at all invocation sites.
case class TypeParameterDecl(name: String, extension: Option[DataType], default: Option[DataType])

object Transformers {
  def makeGenerics(objectName: Option[String], typeParameters: Option[Seq[TypeParameterDecl]])(implicit
      ctx: TransformContext
  ) =
    typeParameters.map(ts => {
      objectName.map(name => ctx.addGlobalTypeDefault(name, ts))
      ts.map(t => Generic(t.name, t.extension))
    })
}

import Transformers._

class TransformContext(module: String) {
  var currentModule = module

  private val companions = Map[String, CompanionObject]()
  def mergeToCompanion(name: String, member: SJSTopLevel) = {
    companions
      .getOrElseUpdate(name, new CompanionObject(name))
      .members += member
  }

  def resetCompanions(): Seq[CompanionObject] = {
    val result = companions.values.toSeq
    companions.clear()
    result
  }

  def withCurrentModule[A](newModule: String)(f: => A): A = {
    val old = currentModule
    currentModule = newModule
    val result = f
    currentModule = old
    result
  }

  val typeMapping = Map[String, Seq[DataType]]()

  def addGlobalTypeDefault(objectName: String, t: Seq[TypeParameterDecl]) = {
    typeMapping += ((objectName, t.flatMap(_.default)))
  }

}

sealed trait TopLevelStatement {
  def transform(implicit ctx: TransformContext): SJSTopLevel
}

// export const foo: A;
case class Constant(
    name: String,
    dataType: DataType,
    var native: Boolean = false
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) =
    new NativeValue(name, dataType, native = native, mutable = false)
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

    if (allStrings.length == options.length && allStrings.length > 0) {
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
        .map[SJSTopLevel](memName => new NativeValue(memName, Base(name), mutable = false))
        .map(mem => ctx.mergeToCompanion(name, mem))

      new NativeTrait(name, None, None, Buffer())
      // new NativeObject(name, s"THREE.$name", members)
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

  def dedupValues(values: Seq[SJSTopLevel]): Seq[SJSTopLevel] = {
    val empty = Seq[SJSTopLevel]()
    val seen = Set[String]()
    val (_, result) = values.foldLeft((seen, empty)) { case ((seen, result), nextValue) =>
      nextValue match {
        case v: NativeValue => {
          if (seen contains v.name) (seen, result)
          else (seen + v.name, result :+ nextValue)
        }
        case _ => (seen, result :+ nextValue)
      }
    }
    result
  }

  def transform(implicit ctx: TransformContext) = {
    ctx.withCurrentModule(name) {
      // - Statics are filtered into companion object
      val fnMems = dedupValues(functions.map(f => Function(f).transform)).toBuffer
      val vals = values.map(_.transform).toBuffer

      // - merge implements and extends clauses
      val exts = {
        (extensions, implements) match {
          case (Some(e), Some(i)) => Some(e :+ i)
          case (Some(e), _)       => Some(e)
          case (_, Some(i))       => Some(Seq(i))
          case _                  => None
        }
      }

      val typeArgs = makeGenerics(Some(name), typeParameters)
      new NativeClass(name, typeArgs, constructors, vals ++ fnMems, exts)
    }
  }
}

// export type Foo = [number, number] | string;
case class TopLevelType(name: String, dataType: DataType) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = new TypeAlias(name, dataType)
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
  def transform(implicit ctx: TransformContext) = {
    ctx.withCurrentModule(name) {
      val typeArgs = makeGenerics(Some(name), parameters)
      val transformMems = members.map(_.transform).toBuffer
      new NativeTrait(name, typeArgs, extensions, transformMems)
    }
  }
}

case class Namespace(
    name: String,
    members: Seq[TopLevelStatement]
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = {
    ctx.withCurrentModule(name) {
      new NativeObject(name, name, members.map(_.transform).toBuffer)
    }
  }
}

case class Function(
    value: FnMember,
    native: Boolean = false
) extends TopLevelStatement {
  def transform(implicit ctx: TransformContext) = {
    val FnMember(name, t, args, ret, getSet, static) = value
    val typeArgs = makeGenerics(None, t) // Don't globally declare these generics

    val returnType = ret.getOrElse(Base("Unit"))

    val fn = getSet match {
      case Some(Getter) => // def $name: ret
        new NativeValue(name, returnType, mutable = false)
      case Some(Setter) =>
        new NativeValue(name, args.args(0).dataType, mutable = true)
      case _ => // def $name<$T>($args) : ret
        new NativeFunction(name, typeArgs, Some(args), returnType, native = native)
    }

    if (static) {
      ctx.mergeToCompanion(ctx.currentModule, fn)
      SJSTopLevel.empty
    } else {
      fn
    }
  }
}
