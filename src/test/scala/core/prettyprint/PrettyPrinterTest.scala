package core.prettyprint

import core.ast.Expr.{Ap, Let, Num, Var}
import core.ast.prettyprint.ProgramPrettyPrinter
import core.ast.{CoreProgram, Supercombinator}
import org.scalatest._

class PrettyPrinterTest extends FlatSpec with Matchers {

  "Pretty printer" should "pretty print a simple program with the let clause" in {
    val input: CoreProgram = List(
      Supercombinator("main", List(), Ap(Var("quadruple"), Num(10))),
      Supercombinator("quadruple", List("x"), Let(
        isRec = false,
        List(("twice_x", Ap(Ap(Var("+"), Var("x")), Var("x")))),
        Ap(Ap(Var("+"), Var("twice_x")), Var("twice_x"))))
    )

    val expected: String =
      """
        |main = quadruple 10 ;
        |quadruple x = let twice_x = x + x
        |              in twice_x + twice_x
        |""".stripMargin.trim

    ProgramPrettyPrinter.prettyPrint(input) should be(expected)
  }
}
