package core.parser

import core.ast.Expr._
import core.ast._

import scala.util.parsing.combinator.JavaTokenParsers

case object Parser extends JavaTokenParsers {

  def parseCoreProgram(program: String): CoreProgram = super.parseAll(pProgram, program) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new IllegalArgumentException(msg)
  }

  def pProgram: Parser[CoreProgram] = oneOrMoreWithSep(pSc, ";")

  def pSc:Parser[Supercombinator[Name]] = (pVar ~ pVar.* <~ "=") ~ pExpr ^^ { case name ~ vars ~ body => Supercombinator(name.name, vars map (_.name), body)}

  def pExpr: Parser[CoreExpr] =  pLet | pCase | pLambda | pExpr1

  def pList: Parser[CoreExpr] = "[" ~> zeroOrMoreWithSep(pExpr, ",") <~ "]" ^^ { exprs => exprs
    .foldRight[CoreExpr](Var("Nil"))((head: CoreExpr, tail: CoreExpr) => Ap(Ap(Var("Cons"), head), tail)) }

  def pAtomic: Parser[CoreExpr] = pPack | "(" ~> pExpr <~ ")" | pVar | pNum | pList

  def pLet: Parser[Expr.Let[Name]] = pLetKeyword ~ (pDefns <~ "in") ~ pExpr ^^ { case keyword ~ defns ~ expr => Expr.Let(keyword, defns, expr) }

  def pDefns: Parser[List[(Name, Expr[Name])]] = oneOrMoreWithSep((pVar <~ "=") ~ pExpr ^^ {case v ~ e => (v.name, e) }, ";")

  def pLetKeyword: Parser[Boolean] = ("letrec" | "let") ^^ { k => k == "letrec"}

  def pLambda: Parser[Expr.Lambda[Name]] = "\\" ~> pVar.+ ~ ("." ~> pExpr) ^^ { case vars ~ expr => Expr.Lambda(vars map { _.name }, expr)}

  def pCase: Parser[Case[Name]] = ("case" ~> pExpr <~ "of") ~ pAlts ^^ { case expr ~ alts => Case(expr, alts) }

  def pAlts: Parser[List[Alter[Name]]] = oneOrMoreWithSep(pAlt, ";")

  def pAlt: Parser[Alter[Name]] = ("<" ~> wholeNumber <~ ">") ~ pVar.* ~ ("->" ~> pExpr) ^^
    { case num ~ vars ~ expr => Alter(num.toInt, vars map { _.name }, expr) }

  type PartialExpr = Option[(String, CoreExpr)]

  def pExpr1: Parser[CoreExpr] = pExpr2 ~ pExpr1c ^^ assembleOp
  def pExpr1c: Parser[PartialExpr] = "||" ~ pExpr1 ^^ foundOp | notFound
  def pExpr2: Parser[CoreExpr] = pExpr3 ~ pExpr2c ^^ assembleOp
  def pExpr2c: Parser[PartialExpr] = "&&" ~ pExpr2 ^^ foundOp | notFound
  def pExpr3: Parser[CoreExpr] = pExpr4 ~ pExpr3c ^^ assembleOp
  def pExpr3c: Parser[PartialExpr] = ("<=" | "<" | "==" | "!=" | ">=" | ">") ~ pExpr4 ^^ foundOp | notFound
  def pExpr4: Parser[CoreExpr] = pExpr5 ~ pExpr4c ^^ assembleOp
  def pExpr4c: Parser[PartialExpr] = "+" ~ pExpr4 ^^ foundOp |  "-" ~ pExpr5 ^^ foundOp | notFound
  def pExpr5: Parser[CoreExpr] = pExpr6 ~ pExpr5c ^^ assembleOp
  def pExpr5c: Parser[PartialExpr] = "*" ~ pExpr5 ^^ foundOp |  "/" ~ pExpr6 ^^ foundOp | notFound
  def pExpr6: Parser[CoreExpr] = pAtomic.+ ^^ {_ reduceLeft ((l, r) => Expr.Ap(l, r))}

  def pPack: Parser[Expr.Constr[Name]] = "Pack{" ~> ((wholeNumber <~ ",") ~ wholeNumber) <~ "}" ^^ { case n1 ~ n2 => Expr.Constr(n1.toInt, n2.toInt)}

  def pNum: Parser[Expr.Num[Name]] = """\d+""".r ^^ { str => Num(str.toInt) }

  def pVar: Parser[Expr.Var[Name]] = ident.withFilter(!Expr.keywords.contains(_)) ^^ { str => Var(str) }

  def oneOrMoreWithSep[T](p: Parser[T], sep: String): Parser[List[T]] =
    p ~ sep  ~ oneOrMoreWithSep(p, sep) ^^ { case head ~ _ ~ tail => head :: tail} | p ^^ {List(_)}

  def zeroOrMoreWithSep[T](p: Parser[T], sep: String): Parser[List[T]] =
    oneOrMoreWithSep(p, sep) | ("" ^^ { _ => Nil })

  def assembleOp(v: ~[CoreExpr, PartialExpr]):CoreExpr = v._2 match {
    case Some((op, rhs)) => Ap(Ap(Var(op), v._1), rhs)
    case None =>v._1
  }
  def foundOp(v: ~[String, CoreExpr]):Option[(String, CoreExpr)] = Option(v._1, v._2)
  def notFound:Parser[Option[(String, CoreExpr)]] = "" ^^ { _ => None }
}
