package core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class InterpreterErrorsTest extends FlatSpec with Matchers {

  behavior of "Interpreter"

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
