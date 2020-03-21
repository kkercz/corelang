package core.interpreter

import core.util.Examples
import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {

  behavior of "Interpreter"

  it should "run some simple programs with function applications only" in {
    Interpreter.compute("main = 42") should be("42")
    Interpreter.compute("main = I 42") should be("42")
    Interpreter.compute("main = S K K 3") should be ("3")
    Interpreter.compute(
    """
        |everything = 42 ;
        |main = I K everything 5""".stripMargin) should be("42")
  }

  it should "run programs with let" in {
    Interpreter.compute("main = let x = 4 ; y = 5 in I K1 x y") should be("5")
  }

  it should "run programs with letrec" in {
    Interpreter.compute(Examples.letrec) should be("4")
  }

  it should "not recompute shared expressions" in {
    Interpreter.runTemplateInstantiation(
      """
        |main = let id1 = I I I
        |in id1 id1 3""".stripMargin).last.stats.reductions should be(5)
  }

  it should "handle arithmetic operations" in {
    Interpreter.compute("main = negate 3") should be("-3")
    Interpreter.compute("main = twice negate 3") should be("3")
    Interpreter.compute("main = negate (I 3)") should be("-3")
    Interpreter.compute("main = (1*2 + 3*5 - 7) / 2") should be("5")
    Interpreter.compute("square x = x * x; main = square (square 3)".stripMargin) should be("81")
  }

  it should "have proper boolean logic" in {
    Interpreter.compute("main = not True") should be("False")
    Interpreter.compute("main = not False") should be("True")
    Interpreter.compute("main = True && True") should be("True")
    Interpreter.compute("main = True && False") should be("False")
    Interpreter.compute("main = False && True") should be("False")
    Interpreter.compute("main = False && False") should be("False")
    Interpreter.compute("main = True || True") should be("True")
    Interpreter.compute("main = True || False") should be("True")
    Interpreter.compute("main = False || True") should be("True")
    Interpreter.compute("main = False || False") should be("False")
  }

  it should "support boolean operators and number comparisons" in {
    Interpreter.compute("main = 1 < 2") should be("True")
    Interpreter.compute("main = 2 > 3") should be("False")
    Interpreter.compute("main = 2 >= 2 && 3 <= 4") should be("True")
    Interpreter.compute("main = 1 != 1 || 2 == 2") should be("True")
  }

  it should "compute factorial with the `if` clause" in {
    Interpreter.compute(
      """
        |fac n = if (n == 0) 1 (n * fac (n-1)) ;
        |main = fac 3""".stripMargin) should be("6")
  }
}
