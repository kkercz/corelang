package core.interpreter.data

import core.ast.{CoreExpr, Name}
import core.prettyprint.PrettyPrinter

sealed trait Node {
  final def isData: Boolean = this match {
    case Node.Num(_) => true
    case Node.App(_, _) => false
    case Node.SC(_, _, _) => false
  }
}

object Node {

  case class App(a1: Address, a2: Address) extends Node {
    override def toString: Name = s"Node.App(a_$a1 a_$a2)"
  }

  case class SC(name: Name, bindings: List[Name], body: CoreExpr) extends Node {
    override def toString: Name = {
      val bindingsString = bindings.mkString(" ")
      val bodyString = PrettyPrinter.prettyPrint(body)
      s"Node.SC($name $bindingsString = $bodyString)"
    }
  }

  case class Num(value: Int) extends Node {
    override def toString: Name = s"Node.Num(value.toString)"
  }

}
