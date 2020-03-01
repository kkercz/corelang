package com.kkercz.core.prettyprint

import com.kkercz.core.ast.CoreProgram
import com.kkercz.core.ast.Expr.{Ap, Let, Num, Var}
import org.scalatest._

class PrettyPrinterTest extends FlatSpec with Matchers {

  "Pretty printer" should "pretty print a simple program with the let clause" in {
    val input: CoreProgram = List(
      ("main", List(), Ap(Var("quadruple"), Num(10))),
      ("quadruple", List("x"), Let(
        isRec = false,
        List(("twice_x", Ap(Ap(Var("+"), Var("x")), Var("x")))),
        Ap(Ap(Var("+"), Var("twice_x")), Var("twice_x"))))
    )

    val expected: String =
      """
        |main = quadruple 10
        |quadruple x = let twice_x = x + x
        |              in twice_x + twice_x
        |""".stripMargin.trim

    PrettyPrinter.prettyPrint(input) should be(expected)
  }
}
