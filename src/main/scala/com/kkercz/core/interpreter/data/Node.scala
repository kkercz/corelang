package com.kkercz.core.interpreter.data

import com.kkercz.core.ast.{CoreExpr, Name}
import com.kkercz.core.prettyprint.PrettyPrinter

trait Node {}

object Node {

  case class App(a1: Address, a2: Address) extends Node {
    override def toString: Name = s"a_$a1 a_$a2"
  }

  case class SC(name: Name, bindings: List[Name], body: CoreExpr) extends Node {
    override def toString: Name = {
      val bindingsString = bindings.mkString(" ")
      val bodyString = PrettyPrinter.prettyPrint(body)
      s"$name $bindingsString = $bodyString"
    }
  }

  case class Num(value: Int) extends Node {
    override def toString: Name = value.toString
  }

}
