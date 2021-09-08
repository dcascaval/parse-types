package tsparse

import fastparse._
import JavaWhitespace._ // Ignore whitespace and //, /* */ comment blocks
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

object Parser {

  // KEYWORDS
  def cls[_: P] = P("class")
  def itf[_: P] = P("interface")
  def typ[_: P] = P("type")
  def exp[_: P] = P("export")
  def enm[_: P] = P("enum")
  def stc[_: P] = P("static")
  def rdo[_: P] = P("readonly")

  // SYNTAX
  def colon[_: P] = P(":")
  def comma[_: P] = P(",")
  def braced[_: P, T](p: => P[T]) = P("{" ~ p ~ "}")
  def withParens[_: P, T](p: => P[T]) = P("(" ~ p ~ ")")

  def colonQ[_: P]: P[Boolean] = P((":" | "?:").!).map(s => s.length() == 2)

  def identColonQ[_: P]: P[(String, Boolean)] = P(
    ident ~ "?".?.! ~ ":"
  ).map { case (name, q) =>
    (name, q.length() > 0)
  }

  // IDENTIFIERS
  def alphaUnder[_: P] = P(CharPred(c => c.isLetter || c == '_'))
  def alphaUnderDigit[_: P] = P(CharsWhile(c => c.isLetterOrDigit || c == '_'))
  def ident[_: P] = P((alphaUnder ~ alphaUnderDigit.?).!)

  // PARAMETERS
  def parameter[_: P]: P[Argument] = P(identColonQ ~ dataType).map { case (name, rdo, data) =>
    Argument(name, data, rdo)
  }
  def argumentList[_: P]: P[Seq[Argument]] = P("(" ~ parameter.rep(0, sep = ",") ~ ",".? ~ ")")

  // TYPES
  //

  // Consume array suffixes after any successful type parse so we don't infinitely recurse
  def withArray[_: P](baseParser: => P[DataType]): P[DataType] =
    P(baseParser ~ "[]".!.rep(0)).map { case (baseType, seq) =>
      seq.foldLeft[DataType](baseType)((t, _) => ArrayOf(t))
    }

  // number, undefined, etc.
  def baseType[_: P]: P[DataType] = P(ident).map(Base(_))
  // 'foo'
  def stringType[_: P]: P[DataType] = P("'" ~/ ident ~ "'").map(StringType(_))
  // Curve<A, B>
  def parameterizedType[_: P]: P[DataType] = P(
    alphaUnderDigit.! ~ "<" ~/ dataType.rep(1, sep = ",") ~ ">"
  ).map { case (name, ps) => Parameterized(name, ps) }

  // A | B | C
  def unionType[_: P]: P[DataType] = withArray(
    P(
      nonUnionType.rep(2, sep = "|"./)
    ).map(mems => Union(Set.from(mems)))
  )

  // arrow (err: Error, data: Uint8Array, final: boolean) => void;
  def arrowType[_: P]: P[DataType] = P(
    argumentList ~/ "=>" ~ dataType
  ).map { case (ps, ret) => Arrow(ps, ret) }
  // [ A , B ]
  def tupleType[_: P]: P[DataType] = P(
    "[" ~/ dataType.rep(1, sep = ",") ~ "]"
  ).map(ConstArray(_))

  def nonUnionType[_: P]: P[DataType] = withArray(
    P(
      arrowType | parameterizedType | baseType |
        tupleType | stringType
    )
  )

  // Any datatype
  def dataType[_: P]: P[DataType] = P(
    NoCut(unionType) | nonUnionType | withArray("(" ~ dataType ~ ")")
  )

  def withEnd[_: P, T](p: => P[T]) = P(p ~ End)

  //

  // STATEMENTS
  def importStmt[_: P] = P("import" ~ CharsWhile(_ != ';') ~ ";")

  def topLevel[_: P]: P[TopLevelStatement] = ???
  def run[_: P] = P(topLevel.rep(0) ~ End)
}

object TestParse extends App {
  import Parser._

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

  println("Success")

}
