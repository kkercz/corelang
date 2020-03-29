package core.parser

import core.ast.Expr.{Ap, Num, Var}
import core.ast.{CoreProgram, Supercombinator}
import core.parser.Parser.parseCoreProgram
import core.prettyprint.ProgramPrettyPrinter.prettyPrint
import core.util.Examples
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  "Parser" should "parse the simplest program" in {
    val input: String =
      """
        |main = double 21 ;
        |double x = x + x
        |""".stripMargin

    val expectedResult: CoreProgram = List(
      Supercombinator("main", List(), Ap(Var("double"), Num(21))),
      Supercombinator("double", List("x"), Ap(Ap(Var("+"), Var("x")), Var("x")))
    )

    parseCoreProgram(input) should be(expectedResult)
  }

  "Parser" should "parse the sieve of Eratosthenes" in {

    val expectedResult =
      """main = printList (take 100 (sieve (from 2))) ;
        |from n = cons n (from (n + 1)) ;
        |sieve xs = case xs of
        |                  <1> -> nil ;
        |                  <2> p ps -> cons p (sieve (filter (nonMultiple p) ps)) ;
        |filter predicate xs = case xs of
        |                             <1> -> nil ;
        |                             <2> p ps -> let rest = filter predicate ps
        |                                         in if (predicate p) (cons p rest) rest ;
        |nonMultiple p n = ((n / p) * p) != n ;
        |take n xs = if (n == 0) nil (case xs of
        |                   <1> -> nil ;
        |                   <2> p ps -> cons p (take (n - 1) ps))""".stripMargin

    prettyPrint(parseCoreProgram(Examples.sieve)) should be (expectedResult)
  }


  it should "have a literal for lists" in {
    prettyPrint(parseCoreProgram("main = []")) should be ("main = Nil")
    prettyPrint(parseCoreProgram("main = [1]")) should be ("main = Cons 1 Nil")
    prettyPrint(parseCoreProgram("main = [1,2,3]")) should be ("main = Cons 1 (Cons 2 (Cons 3 Nil))")
    prettyPrint(parseCoreProgram("main = [1+2,I 2,3]")) should be ("main = Cons (1 + 2) (Cons (I 2) (Cons 3 Nil))")
  }
}
