package core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {

  behavior of "Interpreter"

  it should "run simple programs with function applications only" in {
    Interpreter().run("main = 42") should be("42")
  }

  it should "run the simplest program with one application" in {
    Interpreter().run("main = I 42") should be("42")
  }

  it should "run the simplest program with multiple applications and supercombinators" in {
    Interpreter().run(
      """
        |everything = 42 ;
        |main = I K everything 5""".stripMargin) should be("42")
  }

  it should "throw an illegal argument exception if too many arguments applied to function" in {
    intercept[IllegalArgumentException] {
      Interpreter().run("main = I 2 2")
    }
  }

  it should "throw an illegal argument exception if too few arguments applied to function" in {
    intercept[IllegalArgumentException] {
      Interpreter().run("main = I")
    }
  }
}
