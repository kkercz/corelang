package core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class InterpreterErrorsTest extends FlatSpec with Matchers {

  behavior of "Interpreter"

  it should "throw an illegal argument exception if too many arguments applied to function" in {
    intercept[IllegalArgumentException] {
      TiInterpreter.compute("main = I 2 2")
    }
  }

  it should "throw an illegal argument exception if too few arguments applied to function" in {
    intercept[IllegalArgumentException] {
      TiInterpreter.compute("main = I")
    }
  }

  it should "throw an illegal argument exception if invalid types are used for built-in operators" in {
    intercept[IllegalArgumentException] {
      TiInterpreter.compute("main = 1 && 2")
      TiInterpreter.compute("main = True < 1")
      TiInterpreter.compute("main = not 1")
      TiInterpreter.compute("main = negate False")
    }
  }

  it should "throw illegal state exception if taking tail of empty list" in {
    intercept[IllegalStateException] {
      TiInterpreter.compute("main = snd (MkPair 1 (tail Nil))".stripMargin)
    }
  }
}
