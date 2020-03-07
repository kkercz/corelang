package com.kkercz.core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {

  "Interpreter" should "run the simplest program" in {
    Interpreter().run("main = 42") should be("42")
  }

  "Interpreter" should "run the simplest program with application" in {
    Interpreter().run("main = I 42") should be("42")
  }

  "Interpreter" should "run the simplest program with multiple applications" in {
    Interpreter().run("main = I (K 42 5)") should be("42")
  }
}
