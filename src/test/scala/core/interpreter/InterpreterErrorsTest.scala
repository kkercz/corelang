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

  it should "throw an illegal argument exception if invalid types are used for built-in operators" in {
    intercept[IllegalArgumentException] {
      Interpreter.compute("main = 1 && 2")
      Interpreter.compute("main = True < 1")
      Interpreter.compute("main = not 1")
      Interpreter.compute("main = negate False")
    }
  }
}
