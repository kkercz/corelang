package core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class InterpreterListHandlingTest extends FlatSpec with Matchers {

  behavior of "Interpreter"


  it should "have built-in lists" in {
    Interpreter.compute("main = head (Cons 1 Nil)") should be("1")
    Interpreter.compute("main = length (tail (Cons 1 Nil))") should be("0")
    Interpreter.compute("main = length Nil") should be("0")
    Interpreter.compute("main = length (Cons 1 Nil)") should be("1")
    Interpreter.compute("main = length (Cons 1 (Cons 2 Nil))") should be("2")
  }

  it should "have built-in functions for lists" in {
    Interpreter.compute("main = len (take 3 [1,2,3,4,5])") should be("3")
    Interpreter.compute("main = head (take 3 [1,2,3,4,5])") should be("1")
    Interpreter.compute("main = len (drop 3 [1,2,3,4,5])") should be("2")
    Interpreter.compute("main = head (drop 3 [1,2,3,4,5])") should be("4")
  }

  it should "be able to handle infinite lists" in {
    Interpreter.compute("""
                          |factor x = Cons x (factor (x + 1)) ;
                          |main = hd (tl (tl (factor 1)))""".stripMargin) should be("3")
    Interpreter.compute("""
                          |infinite x = letrec xs = Cons x xs in xs ;
                          |main = hd (tl (tl (infinite 4)))""".stripMargin) should be("4")
  }
}
