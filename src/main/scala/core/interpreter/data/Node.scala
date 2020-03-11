package core.interpreter.data

import core.ast.{CoreExpr, Name}

sealed trait Node {
  final def isData: Boolean = this match {
    case Node.Num(_) => true
    case _ => false
  }
}

object Node {

  case class App(a1: Address, a2: Address) extends Node

  case class SC(name: Name, bindings: List[Name], body: CoreExpr) extends Node

  case class Num(value: Int) extends Node

  case class Ref(a: Address) extends Node

}
