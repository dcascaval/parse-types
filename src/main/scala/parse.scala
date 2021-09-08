package tsparse

import scala.collection.mutable.ArrayBuffer

import fastparse._
import JavaWhitespace._ // Ignore whitespace and //, /* */ comment blocks
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

object Parser {

  // SYNTAX
  def colon[_: P] = P(":")
  def comma[_: P] = P(",")
  def braced[_: P, T](p: => P[T]) = P("{" ~ p ~ "}")
  def withParens[_: P, T](p: => P[T]) = P("(" ~ p ~ ")")

  def identColonQ[_: P]: P[(String, Boolean)] = P(
    ident ~ "?".?.! ~ ":"
  ).map { case (name: String, q: String) => (name, q.length() > 0) }

  // IDENTIFIERS
  def alphaUnder[_: P] = P(CharPred(c => c.isLetter || c == '_'))
  def alphaUnderDigit[_: P] = P(CharsWhile(c => c.isLetterOrDigit || c == '_'))
  def ident[_: P] = P((alphaUnder ~ alphaUnderDigit.?).!)

  // PARAMETERS
  def parameter[_: P]: P[Argument] =
    P(identColonQ ~ dataType).map { case (name: String, optional: Boolean, data: DataType) =>
      Argument(name, data, optional)
    }
  def argumentList[_: P]: P[Seq[Argument]] = P("(" ~ parameter.rep(0, sep = ",") ~ ",".? ~ ")")

  //
  // TYPES
  //
  // Consume array suffixes after any successful type parse so we don't infinitely recurse
  def withArray[_: P](baseParser: => P[DataType]): P[DataType] =
    P(baseParser ~ "[]".!.rep(0)).map { case (baseType: DataType, seq: Seq[String]) =>
      seq.foldLeft[DataType](baseType)((t, _) => ArrayType(t))
    }

  // number, undefined, etc.
  def baseType[_: P]: P[DataType] = P(ident).map(Base(_))
  // 'foo'
  def stringType[_: P]: P[DataType] = P("'" ~/ ident ~ "'").map(StringType(_))
  // Curve<A, B>
  def parameterizedType[_: P]: P[DataType] = P(
    alphaUnderDigit.! ~ "<" ~/ dataType.rep(1, sep = ",") ~ ">"
  ).map { case (name: String, ps: Seq[DataType]) => Parameterized(name, ps) }

  // A | B | C
  def unionType[_: P]: P[DataType] = withArray(
    P(
      "|".? ~ nonUnionType.rep(2, sep = "|"./)
    ).map(mems => UnionType(Set.from(mems)))
  )

  // arrow (err: Error, data: Uint8Array, final: boolean) => void;
  def arrowType[_: P]: P[DataType] = P(
    argumentList ~/ "=>" ~ dataType
  ).map { case (args: Seq[Argument], ret: DataType) => ArrowType(args, ret) }
  // [ A , B ]
  def tupleType[_: P]: P[DataType] = P(
    "[" ~/ dataType.rep(1, sep = ",") ~ "]"
  ).map(TupleType(_))

  def interfaceParameter[_: P] = P(parameter ~ ";")
  def interfaceKey[_: P] = P("[" ~/ ident ~ ":" ~ dataType ~ "]" ~ ":" ~ dataType ~ ";").map {
    case (name, keyType, returnType) => Key(name, keyType, returnType)
  }

  def objectType[_: P]: P[DataType] = P(
    "{" ~/ (interfaceParameter | interfaceKey).rep(0) ~ "}"
  ).map(mems => {
    val args = new ArrayBuffer[Argument]()
    val keys = new ArrayBuffer[Key]()
    for (mem <- mems) {
      mem match {
        case a: Argument => args += a
        case k: Key      => keys += k
      }
    }
    ObjectType(args.toSeq, keys.toSeq)
  })

  def nonUnionType[_: P]: P[DataType] = withArray(
    P(
      arrowType | parameterizedType | baseType |
        tupleType | stringType | objectType
    )
  )

  // Any datatype
  def dataType[_: P]: P[DataType] = P(
    NoCut(unionType) | nonUnionType | withArray("(" ~ dataType ~ ")")
  )

  // <Foo extends A = B>
  def typeArgument[_: P]: P[TypeParameterDecl] = P(
    ident ~ ("extends" ~/ dataType).? ~ ("=" ~/ dataType).?
  ).map { case (name: String, ext: Option[DataType], default: Option[DataType]) =>
    TypeParameterDecl(name, ext, default)
  }

  def typeArgumentList[_: P] = P("<" ~/ typeArgument.rep(1, sep = ",") ~ ">")

  def extensionClause[_: P]: P[Seq[DataType]] = P("extends" ~/ dataType.rep(1, sep = ","))
  def implementsClause[_: P]: P[DataType] = P("implements" ~/ dataType)

  // MEMBERS
  def getSet[_: P] = P(("get" | "set").!).map(str => if (str == "get") Getter else Setter)
  def valueMember[_: P] = P("readonly".!.? ~ identColonQ ~ dataType ~ ";").map {
    case (readOnly, (name, optional), typ) =>
      ValueMember(name, typ, optional, readOnly.isDefined)
  }
  def staticMember[_: P] = ("static" ~/ "readonly".!.? ~ ident ~ dataType ~ ";").map {
    case (readOnly, name, typ) =>
      StaticMember(name, typ, readOnly.isDefined) // never optional
  }
  def functionMember[_: P] = P(getSet.? ~ ident ~ argumentList ~/ ":" ~ dataType ~ ";").map {
    case (gs, name, args, ret) =>
      FnMember(name, args, ret, gs)
  }
  def constructor[_: P] = P("constructor" ~/ argumentList ~ ";").map(Constructor(_))
  def classMember[_: P] = P(functionMember | staticMember | valueMember | constructor)

  // STATEMENTS
  def importStmt[_: P] = P("import" ~ CharsWhile(_ != ';') ~ ";")

  def topConstant[_: P] = P("export" ~ "const" ~/ ident ~ ":" ~ dataType ~ ";").map { case (name, typ) =>
    TopLevelConstant(name, typ)
  }

  def topInterface[_: P] = P(
    "export" ~ "interface" ~/ ident ~ typeArgumentList.? ~ extensionClause.? ~ "{" ~
      valueMember.rep(0) ~ "}"
  ).map { case (name, args, exts, members) =>
    new Interface(name, args, members, exts)
  }

  def topClass[_: P] = P(
    "export" ~ "class" ~/ ident ~ typeArgumentList.? ~ extensionClause.? ~ implementsClause.? ~ "{" ~
      classMember.rep(0) ~ "}"
  ).map { case (name, typeArgs, extensions, implements, members) =>
    val values = ArrayBuffer[ValueMember]()
    val statics = ArrayBuffer[StaticMember]()
    val functions = ArrayBuffer[FnMember]()
    val ctrs = ArrayBuffer[Constructor]()
    for (mem <- members) {
      mem match {
        case v: ValueMember  => values += v
        case s: StaticMember => statics += s
        case f: FnMember     => functions += f
        case c: Constructor  => ctrs += c
      }
    }

    new BasicClass(
      name,
      typeArgs,
      values.toSeq,
      statics.toSeq,
      functions.toSeq,
      ctrs.toSeq,
      extensions,
      implements
    )
  }

  def topType[_: P] = P(
    "export" ~ "type" ~/ ident ~ "=" ~ dataType ~ ";"
  ).map { case (name, typ) => TopLevelType(name, typ) }

  def digits[_: P] = P(CharsWhile(_.isDigit).!).map(_.toInt)

  def enumString[_: P] = P("'" ~/ ident ~ "'").map(StringMem(_))
  def enumValue[_: P] = P(ident ~ "=" ~/ digits).map { case (name, value) => ValueMem(name, value) }
  def enumBasic[_: P] = P(ident).map(BasicMember(_))

  def enumMember[_: P] = P(enumValue | enumString | enumBasic)

  def topEnum[_: P] = P(
    "export" ~ "enum" ~/ ident ~ "{" ~ enumMember.rep(0) ~ "}"
  ).map { case (name, mems) =>
    TopLevelEnum(name, mems)
  }
  //
  //
  //
  def withEnd[_: P, T](p: => P[T]) = P(p ~ End)

  def topLevelStmt[_: P]: P[TopLevelStatement] = (
    topEnum | topInterface | topClass | topConstant | topType
  )

  def allTop[_: P]: P[Seq[TopLevelStatement]] = (
    (importStmt | topLevelStmt)
      .rep(0)
    )
    .map(stmts =>
      stmts.flatMap(s =>
        s match {
          case impt: Unit              => None
          case stmt: TopLevelStatement => Some(stmt)
        }
      )
    )

  def run[_: P] = P(topLevelStmt.rep(0) ~ End)
}

object TestParse extends App {
  import Parser._
  import TestHelpers._

  BaseTests.run

  withPrint {
    parseWhole(
      """export interface MorphTarget {
      name: string;
      vertices: Vector3[];
  }""",
      topInterface(_)
    )
  }

  println("Success")

}

// TODO:
//   - type parameters for arrow
//   - varargs
//   - optional interface function types???
//   - special casing

object BaseTests {
  import Parser._
  import TestHelpers._

  def run() = {
    val exImport = "import { BufferAttribute } from './../core/BufferAttribute';"
    parseWhole(exImport, importStmt(_))

    info("idents")
    parseWhole("fff", ident(_))
    parseWhole("fff", baseType(_))
    parseWhole("fff", dataType(_))

    info("arrays")
    parseWhole("number[]", dataType(_))
    parseWhole("number[][]", dataType(_))
    parseWhole("(number)[][]", dataType(_))

    info("parens")
    parseWhole("(fff)", dataType(_))

    info("unions")
    parseWhole("number | undefined", unionType(_))
    parseWhole("number | undefined | string | what", unionType(_))
    parseWhole("number | undefined", dataType(_))
    parseWhole("number | undefined | string | what", dataType(_))

    info("parameters")
    parseWhole("k: number", parameter(_))

    info("arrows")
    parseWhole("() => void", arrowType(_))
    parseWhole("(k: number) => void", arrowType(_))
    parseWhole("(i: k, j?: q) => fff", arrowType(_))
    parseWhole("(kk: k[], jolly_roger?: q) => fff", arrowType(_))
    parseWhole("(kk: No[], jolly_roger?: Yes) => fff", dataType(_))
    parseWhole("() => void", dataType(_))

    info("paramTypes")
    parseWhole("Curve<K>", dataType(_))
    parseWhole("Curve<K,Z[]>", dataType(_))
    parseWhole("A<B<C>>", dataType(_))
    parseWhole("K<L<M>,N|Z,()=>O>", dataType(_))

    info("constArrays")
    parseWhole("[number, number]", dataType(_))
    parseWhole("[number, K<F>[][]]", dataType(_))
    parseWhole("[[number], o[][]]", dataType(_))

    info("objects")
    parseWhole("{}", dataType(_))
    parseWhole("{ uniform: bool; test?: case; }", dataType(_))
    parseWhole("{ uniform: { DISTANCE: {}; }; test?: case; }", dataType(_))
    parseWhole("{ [key: number]: IUniform; test?: case; }", dataType(_))

    info("integration")
    val tts = Seq(
      "number | undefined",
      "(number | undefined)[]",
      "number[] | undefined",
      "'foo' | 'bar'",
      """(segment: LightningSegment[],
                 parentSubray: () => Lightning[],
                 childSubray?: (ff: K) => 'string' | void,
                 lightningStrike: LightningStrike,
             ) => void"""
    )

    def dtEnd[_: P] = withEnd(dataType)

    for (tt <- tts) {
      parseWhole(tt, dtEnd(_))
    }
  }
}

object TestHelpers {
  var print = false
  def withPrint(f: => Any) = {
    print = true
    f
    print = false
  }

  def parseWhole[T](str: String, p: P[_] => P[T]): Option[T] = {
    // println(str)
    val res = fastparse.parse(str, p)
    if (print) {
      println(s"$str => $res")
    }
    res match {
      case Success(value, index) =>
        assert(index == str.length())
        Some(value)
      case f: Failure =>
        println(f.trace().longMsg)
        None
    }
  }

  def info(str: String) =
    println(s"\n> ${str.toUpperCase()} ${"-" * (100 - str.length())} \n")
}
