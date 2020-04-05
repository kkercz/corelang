package core.interpreter

import org.scalatest.{FlatSpec, Matchers}

class InterpreterListHandlingTest extends FlatSpec with Matchers {

  behavior of "Interpreter"

  it should "have built-in lists" in {
    TiInterpreter.compute("main = head (Cons 1 Nil)") should be("1")
    TiInterpreter.compute("main = length (tail (Cons 1 Nil))") should be("0")
    TiInterpreter.compute("main = length Nil") should be("0")
    TiInterpreter.compute("main = length (Cons 1 Nil)") should be("1")
    TiInterpreter.compute("main = length (Cons 1 (Cons 2 Nil))") should be("2")
  }

  it should "have built-in functions for lists" in {
    TiInterpreter.compute("main = len (take 3 [1,2,3,4,5])") should be("3")
    TiInterpreter.compute("main = head (take 3 [1,2,3,4,5])") should be("1")
    TiInterpreter.compute("main = len (drop 3 [1,2,3,4,5])") should be("2")
    TiInterpreter.compute("main = head (drop 3 [1,2,3,4,5])") should be("4")
    TiInterpreter.compute("main = head (tail (map (λ a . a * 2) [1,2,3]))".stripMargin) should be("4")
  }

  it should "be able to handle infinite lists" in {
    TiInterpreter.compute("""
                          |factor x = Cons x (factor (x + 1)) ;
                          |main = hd (tl (tl (factor 1)))""".stripMargin) should be("3")
    TiInterpreter.compute("""
                          |infinite x = letrec xs = Cons x xs in xs ;
                          |main = hd (tl (tl (infinite 4)))""".stripMargin) should be("4")
    TiInterpreter.compute("""
                          |add a b = a + b ;
                          |infinite x = Cons x (infinite x) ;
                          |doubleInfinite x = zipWith add (infinite x) (infinite x) ;
                          |main = hd (drop 5 (doubleInfinite 4))""".stripMargin) should be("8")
  }

  it should "be able to print lists" in {
    TiInterpreter.compute("main = printList []".stripMargin) should be("")
    TiInterpreter.compute("main = printList [1,2,3]".stripMargin) should be("1 2 3")
  }

  it should "be able to print the fibonacci sequence recursive list" in {
    TiInterpreter.compute("""
                          |fib = Cons 0 (Cons 1 (zipWith (\a b.a+b) fib (tail fib))) ;
                          |main = printList (take 10 (drop 1 fib))""".stripMargin) should be("1 1 2 3 5 8 13 21 34 55")
  }
}
