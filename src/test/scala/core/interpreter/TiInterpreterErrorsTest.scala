package core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class TiInterpreterErrorsTest extends FlatSpec with Matchers {

  behavior of "Template instantiation Interpreter"

  it should "throw an illegal argument exception if too many arguments applied to function" in {
    intercept[IllegalArgumentException] {
      Interpreter.Ti.compute("main = I 2 2")
    }
  }

  it should "throw an illegal argument exception if too few arguments applied to function" in {
    intercept[IllegalArgumentException] {
      Interpreter.Ti.compute("main = I")
    }
  }

  it should "throw an illegal argument exception if invalid types are used for built-in operators" in {
    intercept[IllegalArgumentException] {
      Interpreter.Ti.compute("main = 1 && 2")
      Interpreter.Ti.compute("main = True < 1")
      Interpreter.Ti.compute("main = not 1")
      Interpreter.Ti.compute("main = negate False")
    }
  }

  it should "throw illegal state exception if taking tail of empty list" in {
    intercept[IllegalStateException] {
      Interpreter.Ti.compute("main = snd (MkPair 1 (tail Nil))".stripMargin)
    }
  }
}
