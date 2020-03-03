package com.kkercz.core.parser

import com.kkercz.core.ast.Expr.{Ap, Case, Num, Var}
import com.kkercz.core.ast._

import scala.util.parsing.combinator.JavaTokenParsers

case object Parser extends JavaTokenParsers {

  def parse(program: String) = super.parseAll(pProgram, program) match {
    case Success(result, _) => result
    case Error(msg, _) => throw new IllegalArgumentException(msg)
  }

  def pProgram: Parser[CoreProgram] = oneOrMoreWithSep(pSc, ";")

  def pSc:Parser[ScDefn[Name]] = (pVar ~ pVar.* <~ "=") ~ pExpr ^^ { case name ~ vars ~ body => (name.name, vars map (_.name), body)}

  def pExpr: Parser[CoreExpr] =  pLet | pCase | pLambda | pExpr1

  def pAtomic: Parser[CoreExpr] = pPack | "(" ~> pExpr <~ ")" | pVar | pNum

  def pLet: Parser[Expr.Let[Name]] = pLetKeyword ~ (pDefns <~ "in") ~ pExpr ^^ { case keyword ~ defns ~ expr => Expr.Let(keyword, defns, expr) }

  def pDefns: Parser[List[(Name, Expr[Name])]] = oneOrMoreWithSep((pVar <~ "=") ~ pExpr ^^ {case v ~ e => (v.name, e) }, ";")

  def pLetKeyword: Parser[Boolean] = ("letrec" | "let") ^^ { k => k == "letrec"}

  def pLambda: Parser[Expr.Lambda[Name]] = "\\" ~> pVar.+ ~ ("." ~> pExpr) ^^ { case vars ~ expr => Expr.Lambda(vars map { _.name }, expr)}

  def pCase: Parser[Case[Name]] = ("case" ~> pExpr <~ "of") ~ pAlts ^^ { case expr ~ alts => Case(expr, alts) }

  def pAlts: Parser[List[Alter[Name]]] = oneOrMoreWithSep(pAlt, ";")

  def pAlt: Parser[Alter[Name]] = ("<" ~> wholeNumber <~ ">") ~ pVar.* ~ ("->" ~> pExpr) ^^
    { case num ~ vars ~ expr => (num.toInt, vars map { _.name }, expr) }

  def pExpr1: Parser[CoreExpr] = pExpr2 ~ "|" ~ pExpr1 ^^ appBinary | pExpr2
  def pExpr2: Parser[CoreExpr] = pExpr3 ~ "&" ~ pExpr2 ^^ appBinary | pExpr3
  def pExpr3: Parser[CoreExpr] = pExpr4 ~ ("<" | "<=" | "==" | "~=" | ">=" | ">") ~ pExpr3 ^^ appBinary | pExpr4
  def pExpr4: Parser[CoreExpr] = pExpr5 ~ "+" ~ pExpr4 ^^ appBinary | pExpr5 ~ "-" ~ pExpr5 ^^ appBinary | pExpr5
  def pExpr5: Parser[CoreExpr] = pExpr6 ~ "*" ~ pExpr5 ^^ appBinary | pExpr6 ~ "/" ~ pExpr6 ^^ appBinary | pExpr6
  def pExpr6: Parser[CoreExpr] = pAtomic.+ ^^ mkAp

  def pPack: Parser[Expr.Constr[Name]] = "Pack{" ~> (wholeNumber ~ wholeNumber) <~ "}" ^^ { case n1 ~ n2 => Expr.Constr(n1.toInt, n2.toInt)}

  def pNum: Parser[Expr.Num[Name]] = wholeNumber ^^ { str => Num(str.toInt) }

  def pVar: Parser[Expr.Var[Name]] = ident.withFilter(!Expr.keywords.contains(_)) ^^ { str => Var(str) }

  def oneOrMoreWithSep[T](p: Parser[T], sep: String): Parser[List[T]] =
    p ~ sep  ~ oneOrMoreWithSep(p, sep) ^^ { case head ~ _ ~ tail => head :: tail} | p ^^ {List(_)}

  def mkAp(value: List[CoreExpr]): CoreExpr = value reduceLeft ((l, r) => Expr.Ap(l, r))
  def appBinary(v: ~[~[CoreExpr, String], CoreExpr]):CoreExpr = Ap(Ap(Var(v._1._2), v._1._1), v._2)
}
