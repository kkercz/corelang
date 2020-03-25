package core.interpreter.data

import core.ast.{CoreExpr, Expr, Name}
import core.interpreter.ti.BuiltInFunction
import core.interpreter.ti.GraphReducer.Env
import core.lang.Prelude

sealed trait Node {
  final def isData: Boolean = this match {
    case Node.Num(_) => true
    case Node.Constr(_, _) => true
    case _ => false
  }

  final def display(): String = this match {
    case Node.Num(value) => value.toString
    case Node.Constr(expr, Nil) if expr == Prelude.trueLiteral => "True"
    case Node.Constr(expr, Nil) if expr == Prelude.falseLiteral => "False"
    case Node.Constr(expr, Nil) if expr == Prelude.nilLiteral => "[]"
    case node => node.toString
  }
}

object Node {

  case class App(a1: Address, a2: Address) extends Node

  case class SC(name: Name, bindings: List[Name], body: CoreExpr) extends Node

  case class Num(value: Int) extends Node

  case class Constr(expr: Expr.Constr[Name], args: List[Address] = List()) extends Node

  case class Case(expr: Address, alternatives: List[Alternative]) extends Node

  case class Ref(a: Address) extends Node

  case class Primitive(op: BuiltInFunction) extends Node

  case class Alternative(tag: Int, argsToExpression: (TiHeap, Env, List[Address]) => (TiHeap, Address))
}
