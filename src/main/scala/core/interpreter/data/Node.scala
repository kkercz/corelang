package core.interpreter.data

import core.ast.{CoreExpr, Expr, Name}
import core.interpreter.ti.BuiltInFunction
import core.lang.Prelude

sealed trait Node {
  final def isData: Boolean = this match {
    case Node.Num(_) => true
    case Node.Constr(_) => true
    case _ => false
  }

  final def display(): String = this match {
    case Node.Num(value) => value.toString
    case Node.Constr(expr) if expr == Prelude.trueLiteral => "True"
    case Node.Constr(expr) if expr == Prelude.falseLiteral => "False"
    case node => node.toString
  }
}

object Node {

  case class App(a1: Address, a2: Address) extends Node

  case class SC(name: Name, bindings: List[Name], body: CoreExpr) extends Node

  case class Num(value: Int) extends Node

  case class Constr(expr: Expr.Constr[Name]) extends Node

  case class Ref(a: Address) extends Node

  case class Primitive(op: BuiltInFunction) extends Node
}
