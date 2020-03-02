package com.kkercz.core.parser

import com.kkercz.core.ast.Expr.{Ap, Case, Num, Var}
import com.kkercz.core.ast.{Alter, CoreExpr, Expr, Name}

import scala.util.parsing.combinator.JavaTokenParsers

case object Parser extends JavaTokenParsers {

  def parse(program: String) = super.parseAll(pExpr, program)

  def pExpr: Parser[CoreExpr] = pNum | pCase | pVar | pInfixOpApplication

  def pCase: Parser[Case[Name]] = "case" ~ pExpr ~ "of" ~ pAlts ^^ { case _ ~ expr ~ _ ~ alts => Case(expr, alts) }

  def pAlts: Parser[List[Alter[Name]]] = oneOrMoreWithSep(pAlt, ";")

  def pAlt: Parser[Alter[Name]] = "<" ~ wholeNumber ~ ">" ~ pVar.* ~ "->" ~ pExpr ^^
    { case _ ~ num ~ _ ~ vars ~ _ ~ expr => (num.toInt, vars map { _.name }, expr) }

  def pInfixOpApplication: Parser[Expr.Ap[Name]] = pExpr ~ pBinaryOp ~ pExpr ^^ { case l ~ op ~ r => Ap(Ap(Var(op), l), r) }

  def pBinaryOp: Parser[String] = "+" | "-" | "*" | "/" | "<" | "<=" | "==" | "~=" | ">=" | ">" | "&" | "|"

  def pNum: Parser[Expr.Num[Name]] = wholeNumber ^^ { str => Num(str.toInt) }

  def pVar: Parser[Expr.Var[Name]] = ident ^^ { str => Var(str) }

  def oneOrMoreWithSep[T](p: Parser[T], sep: String): Parser[List[T]] =
    p ~ sep  ~ oneOrMoreWithSep(p, sep) ^^ { case head ~ _ ~ tail => head :: tail} | p ^^ {List(_)}
}
