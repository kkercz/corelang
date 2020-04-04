package core.interpreter.gm.data

import core.ast.Name

sealed trait Instruction {}

object Instruction {
  case object Unwind extends Instruction
  case object MkApp extends Instruction
  case class PushGlobal(name: Name) extends Instruction
  case class PushInt(num: Int) extends Instruction
  case class Push(n: Int) extends Instruction
  case class Slide(n: Int) extends Instruction
}


