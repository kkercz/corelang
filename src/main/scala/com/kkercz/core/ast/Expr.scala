package com.kkercz.core.ast

import com.kkercz.core.ast.Types.{Alter, IsRecursive, Name}

sealed trait Expr[+T]

case class EVar[T](name: Name) extends Expr[T]

case class ENum[T](value: Int) extends Expr[T]

case class EConstr[T](tag: Int, arity: Int) extends Expr[T]

case class EAp[T](lhs: Expr[T], rhs: Expr[T]) extends Expr[T]

case class ELet[T](
                    isRec: IsRecursive,
                    definitions: List[(T, Expr[T])],
                    body: Expr[T]
                  ) extends Expr[T]

case class ECase[T](
                     expr: Expr[T],
                     alternatives: List[Alter[T]]
                   ) extends Expr[T]

case class ELam[T](variables: List[T], body: Expr[T]) extends Expr[T]