package com.kkercz.core.interpreter.ti

import com.kkercz.core.ast.Name

trait Node {}

object Node {

  case class App(a1: Node, a2: Node)

  case class SC(name: Name, bindings: List[Name], body: Node)

  case class Num(value: Int)

}


