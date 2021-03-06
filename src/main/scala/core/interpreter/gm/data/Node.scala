package core.interpreter.gm.data

import core.interpreter.data.Address
import core.interpreter.gm.GmCode

sealed trait Node {
  def display(): String = toString
}

object Node {
  case class Num(int: Int) extends Node {
    override def display(): String = int.toString
  }
  case class Ap(a1: Address, a2: Address) extends Node
  case class Global(arity: Int, code: GmCode) extends Node
}
