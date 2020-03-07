package com.kkercz.core.parser

import com.kkercz.core.ast.Expr.{Ap, Num, Var}
import com.kkercz.core.ast.{CoreProgram, Supercombinator}
import com.kkercz.core.prettyprint.PrettyPrinter
import com.kkercz.core.util.Examples
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

    Parser.parseCoreProgram(input) should be(expectedResult)
  }

  "Parser" should "parse the sieve of Eratosthenes" in {

    val expectedResult =
      """main = take 3 (sieve (from 2)) ;
        |from n = cons n (from (n + 1)) ;
        |sieve xs = case xs of
        |                  <1> -> nil ;
        |                  <2> p ps -> cons p (sieve (filter (nonMultiple p) ps)) ;
        |filter predicate xs = case xs of
        |                             <1> -> nil ;
        |                             <2> p ps -> let rest = filter predicate ps
        |                                         in if (predicate p) (cons p rest) rest ;
        |nonMultiple p n = ((n / p) * p) ~= n ;
        |take n xs = if (n == 0) nil (case xs of
        |                   <1> -> nil ;
        |                   <2> p ps -> cons p (take (n -1) ps))""".stripMargin

    PrettyPrinter.prettyPrint(Parser.parseCoreProgram(Examples.sieve)) should be (expectedResult)
  }
}
