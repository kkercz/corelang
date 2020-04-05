package core.interpreter

import core.util.Examples
import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {

  behavior of "Interpreter"

  it should "run some simple programs with function applications only" in {
    GmInterpreter.compute("main = 42") should be("42")
    GmInterpreter.compute("main = I 42") should be("42")
    GmInterpreter.compute("main = S K K 3") should be ("3")
    GmInterpreter.compute(
    """
        |everything = 42 ;
        |main = I K everything 5""".stripMargin) should be("42")
  }

  it should "run programs with let" in {
    TiInterpreter.compute("main = let x = 4 ; y = 5 in I K1 x y") should be("5")
  }

  it should "run programs with letrec" in {
    TiInterpreter.compute(Examples.letrec) should be("4")
  }

  it should "not recompute shared expressions" in {
    TiInterpreter.runTemplateInstantiation(
      """
        |main = let id1 = I I I
        |in id1 id1 3""".stripMargin).last.stats.reductions should be(5)
  }

  it should "handle arithmetic operations" in {
    TiInterpreter.compute("main = negate 3") should be("-3")
    TiInterpreter.compute("main = twice negate 3") should be("3")
    TiInterpreter.compute("main = negate (I 3)") should be("-3")
    TiInterpreter.compute("main = (1*2 + 3*5 - 7) / 2") should be("5")
    TiInterpreter.compute("square x = x * x; main = square (square 3)".stripMargin) should be("81")
    TiInterpreter.compute("minus1 n = n-1 ; main = minus1 3") should be("2")
  }

  it should "have proper boolean logic" in {
    TiInterpreter.compute("main = not True") should be("False")
    TiInterpreter.compute("main = not False") should be("True")
    TiInterpreter.compute("main = True && True") should be("True")
    TiInterpreter.compute("main = True && False") should be("False")
    TiInterpreter.compute("main = False && True") should be("False")
    TiInterpreter.compute("main = False && False") should be("False")
    TiInterpreter.compute("main = True || True") should be("True")
    TiInterpreter.compute("main = True || False") should be("True")
    TiInterpreter.compute("main = False || True") should be("True")
    TiInterpreter.compute("main = False || False") should be("False")
  }

  it should "support boolean operators and number comparisons" in {
    TiInterpreter.compute("main = 1 < 2") should be("True")
    TiInterpreter.compute("main = 2 > 3") should be("False")
    TiInterpreter.compute("main = 2 >= 2 && 3 <= 4") should be("True")
    TiInterpreter.compute("main = 1 != 1 || 2 == 2") should be("True")
  }

  it should "evaluate case expressions" in {
    TiInterpreter.compute("main = case Pack{1,0} of <1> -> 3") should be("3")
    TiInterpreter.compute("main = case Pack{2,0} of <1> -> 3 ; <2> -> 4") should be("4")
    TiInterpreter.compute("main = case Pack{1,2} 3 2 of <1> a b -> a - b") should be("1")
  }

  it should "compute factorial with the `if` clause" in {
    TiInterpreter.compute(
      """
        |fac n = if (n == 0) 1 (n * fac (n - 1)) ;
        |main = fac 10""".stripMargin) should be("3628800")
  }

  it should "have built-in functions for pairs" in {
    TiInterpreter.compute("main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))") should be("2")
  }

  it should "be lazy, so tail of empty list is never evaluated if not needed" in {
    TiInterpreter.compute("main = fst (MkPair 1 (tail Nil))".stripMargin) should be("1")
  }

  it should "evaluate lambda expressions" in {
    TiInterpreter.compute("main = (λ a b . a - b) 3 2".stripMargin) should be("1")
  }

  it should "evaluate the sieve of Eratosthenes" in {
    TiInterpreter.compute(Examples.sieveUsingPrelude) should be("2 3 5 7 11 13 17 19 23 29")
  }
}
