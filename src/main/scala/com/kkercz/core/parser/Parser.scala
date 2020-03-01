package com.kkercz.core.parser

import com.kkercz.core.ast.Expr.Num
import com.kkercz.core.ast.{Expr, Name}

import scala.util.parsing.combinator.JavaTokenParsers

case object Parser extends JavaTokenParsers {
  def parse(program: String) = super.parse(pNum, program)

  def pNum: Parser[Expr.Num[Name]] = wholeNumber ^^ { str => Num(str.toInt) }
}
