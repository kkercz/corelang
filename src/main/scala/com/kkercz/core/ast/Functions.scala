package com.kkercz.core.ast

case object Functions {
  def bindersOf[T](definitions: List[(T, Expr[T])]): List[T] = definitions.map(d => d._1)

  def rhssOf[T](definitions: List[(T, Expr[T])]): List[Expr[T]] = definitions.map(d => d._2)

  def isAtomic[T](expr: Expr[T]): Boolean = expr match {
    case EVar(name) => true
    case ENum(value) => true
    case _ => false
  }
}
