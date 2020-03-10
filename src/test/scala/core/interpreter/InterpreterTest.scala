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
}
