package core.ast

import core.ast.Expr.{Constr, Num, Var}

sealed trait Expr[+T] {
  def isAtomic: Boolean = this match {
    case Var(_)       => true
    case Num(_)       => true
    case Constr(_, _) => true
    case _            => false
  }
}

object Expr {

  val keywords = Set("let", "letrec", "case", "in", "of", "Pack")
  val operators = Set("||", "&&", "<", "<=", "==", "!=", ">=", ">", "+", "-", "*", "/")

  case class Var[T](name: T) extends Expr[T]

  case class Num[T](value: Int) extends Expr[T]

  case class Constr[T](tag: Int, arity: Int) extends Expr[T]

  case class Ap[T](lhs: Expr[T], rhs: Expr[T]) extends Expr[T]

  case class Let[T](
                     isRec: Boolean,
                     definitions: List[(T, Expr[T])],
                     body: Expr[T]
                   ) extends Expr[T]

  case class Case[T](
                      expr: Expr[T],
                      alternatives: List[Alter[T]]
                    ) extends Expr[T]

  case class Alter[T](tag: Int, args: List[T], body: Expr[T])

  case class Lambda[T](variables: List[T], body: Expr[T]) extends Expr[T]

  // Utility Functions

  def bindersOf[T](definitions: List[(T, Expr[T])]): List[T] = definitions.map(d => d._1)

  def rhssOf[T](definitions: List[(T, Expr[T])]): List[Expr[T]] = definitions.map(d => d._2)



}

