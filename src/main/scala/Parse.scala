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
  def withEnd[_: P, T](p: => P[T]) = P(p ~ End)

  def identColonQ[_: P]: P[(String, Boolean)] = P(ident ~ colonQ)

  def colonQ[_: P]: P[Boolean] = P("?".!.? ~ ":").map(str => str.isDefined)
  def maybeEq[_: P]: P[Boolean] = P("=").map(str => false) // Not optional

  // hack: sometimes ts defns use "= true" instead of ": true"
  def identColonQorEq[_: P]: P[(String, Boolean)] = P(ident ~ (colonQ | maybeEq))

  // IDENTIFIERS
  def alphaUnder[_: P] = P(CharPred(c => c.isLetter || c == '_'))
  def alphaUnderDigit[_: P] = P(CharPred(c => c.isLetterOrDigit || c == '_'))
  def ident[_: P] = P(CharsWhile(c => c.isLetterOrDigit || c == '_').!)

  def alphaUnderDigitDot[_: P] = P(CharPred(c => c.isLetterOrDigit || c == '_' || c == '.'))
  def numDotIdent[_: P] = P(alphaUnderDigitDot.rep(1).!)

  def strChars[_: P] = P(CharsWhile(c => c != '\''))
  def string[_: P] = P("'" ~/ strChars.! ~ "'").map(s => s"'$s'")

  // PARAMETERS
  def parameter[_: P]: P[Argument] =
    P(identColonQ ~ dataType).map { case (name: String, optional: Boolean, data: DataType) =>
      Argument(name, data, optional)
    }

  def argumentList[_: P]: P[ArgList] = P(
    "(" ~ parameter.rep(0, sep = ",") ~
      (",".? ~ "..." ~/ ident ~ ":" ~ dataType).? ~ ",".? ~ ")"
  ).map { case (args, varArg) =>
    ArgList(args, varArg.map { case (name, t) => Argument(name, t, false) })
  }

  //
  // TYPES
  //
  //

  // Consume array suffixes after any successful type parse so we don't infinitely recurse
  def withArray[_: P](baseParser: => P[DataType]): P[DataType] =
    P(baseParser ~ "[]".!.rep(0)).map { case (baseType: DataType, seq: Seq[String]) =>
      seq.foldLeft[DataType](baseType)((t, _) => ArrayType(t))
    }

  // number, undefined, etc.
  // TODO: do we need to split this on dots into accesses?
  def baseType[_: P]: P[DataType] = P(numDotIdent).map(Base(_))

  // 'foo'
  def stringType[_: P]: P[DataType] = P(string).map(StringType(_))

  // Curve<A, B>
  def parameterizedType[_: P]: P[DataType] = P(
    numDotIdent ~ "<" ~/ dataType.rep(1, sep = ",") ~ ">"
  ).map { case (name: String, ps: Seq[DataType]) => Parameterized(name, ps) }

  // A | B | C
  def unionType[_: P]: P[DataType] = withArray(
    P(
      "|".? ~ nonRepeatingType.rep(2, sep = "|"./)
    ).map(mems => UnionType(mems))
  )

  // A & B & C
  def intersectionType[_: P]: P[DataType] = withArray(
    P(
      nonRepeatingType.rep(2, sep = "&"./)
    ).map(mems => IntersectionType(mems))
  )

  // arrow (err: Error, data: Uint8Array, final: boolean) => void;
  def arrowType[_: P]: P[DataType] = P(
    typeArgumentList.? ~ argumentList ~/ "=>" ~ dataType
  ).map { case (typPars, args, ret) => ArrowType(typPars, args, ret) }

  // [ A , B ]
  def tupleType[_: P]: P[DataType] = P(
    "[" ~/ dataType.rep(1, sep = ",") ~ ",".? ~ "]"
  ).map(TupleType(_))

  def interfaceParameter[_: P] = P(parameter)
  def interfaceKey[_: P] = P("[" ~/ ident ~ ":" ~ dataType ~ "]" ~ ":" ~ dataType).map {
    case (name, keyType, returnType) => Key(name, keyType, returnType)
  }
  def interfaceFunction[_: P] = P(
    ident ~ argumentList ~/ ":" ~ dataType
  ).map { case (name, args, returnType) =>
    Argument(name, ArrowType(None, args, returnType), false)
  }

  // { a: Foo, b: number[] }
  def objectType[_: P]: P[DataType] = P(
    "{" ~/ (interfaceParameter | interfaceKey | interfaceFunction).rep(0, sep = ";") ~ ";".? ~ "}"
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

  def nonRepeatingType[_: P]: P[DataType] = withArray(
    P(
      arrowType | parameterizedType | baseType |
        tupleType | stringType | objectType | ("(" ~ dataType ~ ")")
    )
  )

  // Any datatype
  def dataType[_: P]: P[DataType] = P(
    NoCut(unionType) | NoCut(intersectionType) | nonRepeatingType
  )

  //
  //  Type Arguments
  //
  // <Foo extends A = B>
  def typeArgument[_: P]: P[TypeParameterDecl] = P(
    ident ~ singleExtensionClause.? ~ singleDefaultClause.?
  ).map { case (name: String, ext: Option[DataType], default: Option[DataType]) =>
    TypeParameterDecl(name, ext, default)
  }
  def typeArgumentList[_: P] = P("<" ~/ typeArgument.rep(1, sep = ",") ~ ",".? ~ ">")

  def singleExtensionClause[_: P]: P[DataType] = P("extends" ~/ dataType)
  def singleDefaultClause[_: P]: P[DataType] = P("=" ~/ dataType)

  def extensionClause[_: P]: P[Seq[DataType]] = P("extends" ~/ dataType.rep(1, sep = ","))
  def implementsClause[_: P]: P[DataType] = P("implements" ~/ dataType)

  //
  // CLASS/OBJECT MEMBERS
  //
  def getSet[_: P] = P(("get " | "set ").!).map(str => if (str == "get ") Getter else Setter)

  def privateMember[_: P] = P("private" ~/ CharsWhile(_ != ';') ~ ";")

  def keyMember[_: P] = P(interfaceKey ~ ";").map(KeyMember(_))

  def valueMember[_: P] = P("static".!.? ~ "readonly".!.? ~ identColonQorEq ~ dataType ~ ";").map {
    case (static, readOnly, (name, optional), typ) =>
      ValueMember(name, typ, optional, readOnly.isDefined, static.isDefined)
  }

  // NB: ignoring optional function members for now as we have no real way to model them in scala.
  def functionMember[_: P] =
    P(
      getSet.? ~ "static".!.? ~ ("protected" | "public").? ~ ident ~ typeArgumentList.? ~ "?".? ~ argumentList ~/ (":" ~ dataType).? ~ ";"
    ).map { case (gs, static, name, typArgs, args, ret) =>
      FnMember(name, typArgs, args, ret, gs, static.isDefined)
    }

  def constructor[_: P] = P("constructor" ~/ argumentList ~ ";").map(Constructor(_))
  def classMember[_: P]: P[Any] = P(constructor | functionMember | valueMember | privateMember)
  def interfaceMember[_: P]: P[InterfaceMember] = P(valueMember | functionMember | keyMember)

  //
  // STATEMENTS
  //
  def importStmt[_: P] = P("import" ~ CharsWhile(_ != ';') ~ ";")
  def indirectExport[_: P] = P("export" ~ "*" ~ "from" ~/ CharsWhile(_ != ';') ~ ";")
  def directExport[_: P] = P("export" ~ "{" ~/ CharsWhile(_ != ';') ~ ";")
  def defaultExport[_: P] = P("export" ~ "default" ~/ CharsWhile(_ != ';') ~ ";")
  def exportStmt[_: P] = P(directExport | indirectExport | defaultExport)

  def nestedConstant[_: P] = P(("const" | "let") ~/ ident ~ ":" ~ dataType ~ ";").map {
    case (name, typ) => Constant(name, typ)
  }

  def nestedFunction[_: P] = P("function" ~/ functionMember).map(Function(_))

  def nestedInterface[_: P] = P(
    "interface" ~/ ident ~ typeArgumentList.? ~ extensionClause.? ~ "{" ~
      interfaceMember.rep(0) ~ "}"
  ).map { case (name, args, exts, members) =>
    new Interface(name, args, members, exts)
  }

  // Ignoring abstract classes
  def nestedClass[_: P] = P(
    "abstract".? ~ "class" ~/ ident ~ typeArgumentList.? ~ extensionClause.? ~ implementsClause.? ~ "{" ~
      classMember.rep(0) ~ "}"
  ).map { case (name, typeArgs, extensions, implements, members) =>
    val values = ArrayBuffer[ValueMember]()
    val functions = ArrayBuffer[FnMember]()
    val ctrs = ArrayBuffer[Constructor]()
    for (mem <- members) {
      mem match {
        case v: ValueMember => values += v
        case f: FnMember    => functions += f
        case c: Constructor => ctrs += c
        case _              => ()
      }
    }
    new Class(
      name,
      typeArgs,
      values.toSeq,
      functions.toSeq,
      ctrs.toSeq,
      extensions,
      implements
    )
  }

  def nestedType[_: P] = P(
    "type" ~/ ident ~ "=" ~ dataType ~ ";"
  ).map { case (name, typ) => TopLevelType(name, typ) }

  def digits[_: P] = P(CharsWhile(_.isDigit).!).map(_.toInt)
  def enumString[_: P] = P(string).map(StringMem(_))
  def enumValue[_: P] = P(ident ~ "=" ~/ digits).map { case (name, value) => ValueMem(name, value) }
  def enumBasic[_: P] = P(ident).map(BasicMember(_))
  def enumMember[_: P] = P(enumValue | enumString | enumBasic)
  def nestedEnum[_: P] = P(
    "enum" ~/ ident ~ "{" ~ enumMember.rep(0, sep = ",") ~ ",".? ~ "}"
  ).map { case (name, mems) =>
    TopLevelEnum(name, mems)
  }

  def namespaceMember[_: P]: P[Option[TopLevelStatement]] = P(
    nestedFunction | nestedClass | nestedInterface | nestedConstant | exportStmt
  ).map(mem =>
    mem match {
      case u: Unit              => None
      case s: TopLevelStatement => Some(s)
    }
  )

  def namespace[_: P] = P("namespace" ~/ ident ~ "{" ~ namespaceMember.rep(0) ~ "}").map {
    case (name, mems) =>
      Namespace(name, mems.flatMap(identity))
  }

  def topConstant[_: P] = P("export" ~ nestedConstant).map(c => Constant(c.name, c.dataType, true))
  def topFunction[_: P] = P(("export" | "declare") ~ nestedFunction).map(f => Function(f.value, true))
  def topClass[_: P] = P("export" ~ nestedClass)
  def topInterface[_: P] = P("export".? ~ nestedInterface)
  def topNamespace[_: P] = P(("export" | "declare") ~ namespace)
  def topType[_: P] = P("export" ~ nestedType)
  def topEnum[_: P] = P("export" ~ nestedEnum)

  def topLevelStmt[_: P]: P[TopLevelStatement] = (
    topEnum | topInterface | topClass | topConstant | topType | topFunction | topNamespace
  )

  def allTop[_: P]: P[Seq[TopLevelStatement]] = (
    (importStmt | exportStmt | topLevelStmt)
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

  def fullFile[_: P] = P(Start ~ allTop ~ End)

  def run(fileText: String) =
    fastparse.parse(fileText, fullFile(_))
}
