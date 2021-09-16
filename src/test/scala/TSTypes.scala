package tsparse

import scala.collection.mutable.ArrayBuffer
import org.scalatest.funsuite._

import fastparse._
import JavaWhitespace._ // Ignore whitespace and //, /* */ comment blocks
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

object TestHelpers {
  var print = false
  def withPrint(f: => Any) = {
    print = true
    f
    print = false
  }

  def parseWhole[T](rawStr: String, p: P[_] => P[T]): Option[T] = {
    val str = rawStr.trim()
    val res = fastparse.parse(str, p)
    if (print) {
      println(s"$str => $res")
    }
    res match {
      case Success(value, index) =>
        if (index != str.length()) {
          println(s"parsed to $index / ${str.length()}")
        }
        assert(index == str.length())
        Some(value)
      case f: Failure =>
        throw new Exception(f.trace().longMsg)
        None
    }
  }

  def info(str: String) =
    println(s"\n> ${str.toUpperCase()} ${"-" * (100 - str.length())} \n")
}

class SingleTest extends AnyFunSuite {
  import Parser._
  import TestHelpers._

  test("single") {
    withPrint {
      parseWhole(
        "setCount(count: number): this;",
        functionMember(_)
      )
    }
  }
}

class BaseTests extends AnyFunSuite {
  import Parser._
  import TestHelpers._

  test("import/export") {
    val exImport = "import { BufferAttribute } from './../core/BufferAttribute';"
    parseWhole(exImport, importStmt(_))

    parseWhole("export * from '../core/Three';", exportStmt(_))
    parseWhole("export { f as Y, foo as Bar };", exportStmt(_))
  }

  test("idents") {
    parseWhole("_", ident(_))
    parseWhole("a", ident(_))
    parseWhole("_00", ident(_))
    parseWhole("fff", ident(_))
    parseWhole("fff", baseType(_))
    parseWhole("fff", dataType(_))
    parseWhole("fffABDCDA010200__", dataType(_))
  }

  test("arrays") {
    parseWhole("number[]", dataType(_))
    parseWhole("number[][]", dataType(_))
    parseWhole("(number)[][]", dataType(_))
  }

  test("parens") {
    parseWhole("(fff)", dataType(_))
  }

  test("unions") {
    parseWhole("number | undefined", unionType(_))
    parseWhole("number | undefined | string | what", unionType(_))
    parseWhole("number | undefined", dataType(_))
    parseWhole("number | undefined | string | what", dataType(_))
    parseWhole("'inline' | 'immersive-vr' | 'immersive-ar'", dataType(_))
  }

  test("intersections") {
    parseWhole("string & {}", dataType(_))
    parseWhole("(string & {})", dataType(_))
    parseWhole("A | (string & {})", dataType(_))
  }

  test("parameters") {
    parseWhole("k: number", parameter(_))
  }

  test("arrows") {
    parseWhole("() => void", arrowType(_))
    parseWhole("(k: number) => void", arrowType(_))
    parseWhole("(i: k, j?: q) => fff", arrowType(_))
    parseWhole("(kk: k[], jolly_roger?: q) => fff", arrowType(_))
    parseWhole("(kk: No[], jolly_roger?: Yes) => fff", dataType(_))
    parseWhole("() => void", dataType(_))
  }

  test("paramTypes") {
    parseWhole("Curve<K>", dataType(_))
    parseWhole("Curve<K,Z[]>", dataType(_))
    parseWhole("A<B<C>>", dataType(_))
    parseWhole("K<L<M>,N|Z,()=>O>", dataType(_))
  }

  test("constArrays") {
    parseWhole("[number, number]", dataType(_))
    parseWhole("[number, K<F>[][]]", dataType(_))
    parseWhole("[[number], o[][]]", dataType(_))
  }

  test("objects") {
    parseWhole("{}", dataType(_))
    parseWhole("{ uniform: bool; test?: case; }", dataType(_))
    parseWhole("{ uniform: { DISTANCE: {}; }; test?: case; }", dataType(_))
    parseWhole("{ [key: number]: IUniform; test?: case; }", dataType(_))
  }

  test("integration") {
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
