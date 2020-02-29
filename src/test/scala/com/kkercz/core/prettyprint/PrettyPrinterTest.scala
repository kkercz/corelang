package com.kkercz.core.prettyprint

import com.kkercz.core.ast.Types.{CoreProgram, nonRecursive}
import com.kkercz.core.ast.{EAp, ELet, ENum, EVar}
import org.scalatest._

class PrettyPrinterTest extends FlatSpec with Matchers {

  "Pretty printer" should "pretty print a simple program with the let clause" in {
    val input: CoreProgram = List(
      ("main", List(), EAp(EVar("quadruple"), ENum(10))),
      ("quadruple", List("x"), ELet(
        nonRecursive,
        List(("twice_x", EAp(EAp(EVar("+"), EVar("x")), EVar("x")))),
        EAp(EAp(EVar("+"), EVar("twice_x")), EVar("twice_x"))))
    )

    val expected: String =
      """
        |main = quadruple 10
        |quadruple x = let twice_x = x + x
        |              in twice_x + twice_x
        |""".stripMargin.trim

    CorePrettyPrinter.prettyPrint(input) should be(expected)
  }
}
