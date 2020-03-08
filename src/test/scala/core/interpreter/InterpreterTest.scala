package core.interpreter

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

  it should "throw an illegal argument exception if too many arguments applied to function" in {
    intercept[IllegalArgumentException] {
      Interpreter.compute("main = I 2 2")
    }
  }

  it should "throw an illegal argument exception if too few arguments applied to function" in {
    intercept[IllegalArgumentException] {
      Interpreter.compute("main = I")
    }
  }
}
