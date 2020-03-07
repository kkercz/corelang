package com.kkercz.core.interpreter

import com.kkercz.core.interpreter.ti.TiInterpreterMark1

trait Interpreter {
  def run(program: String): String
}

object Interpreter {
  def apply(): Interpreter = (program: String) => TiInterpreterMark1.run(program)
}
