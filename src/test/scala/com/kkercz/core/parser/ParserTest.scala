package com.kkercz.core.parser

import com.kkercz.core.ast.CoreProgram
import com.kkercz.core.ast.Expr.{Ap, Num, Var}
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  "Parser" should "parse the simplest program" in {
    val input: String =
      """
        |main = double 21 ;
        |double = x + x
        |""".stripMargin

    val expectedResult: CoreProgram = List(
      ("main", List(), Ap(Var("double"), Num(21))),
      ("double", List("x"), Ap(Ap(Var("+"), Var("x")), Var("x")))
    )

    Parser.parse(input) should be(expectedResult)
  }
}
