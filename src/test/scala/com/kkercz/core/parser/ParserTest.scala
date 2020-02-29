package com.kkercz.core.parser

import com.kkercz.core.ast.Types.CoreProgram
import com.kkercz.core.ast.{EAp, ENum, EVar}
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  "Parser" should "parse the simplest program" in {
    val input: String =
      """
        |main = double 21 ;
        |double = x + x
        |""".stripMargin

    val expectedResult: CoreProgram = List(
      ("main", List(), EAp(EVar("double"), ENum(21))),
      ("double", List("x"), EAp(EAp(EVar("+"), EVar("x")), EVar("x")))
    )

    Parser.parse(input) should be(expectedResult)
  }
}
