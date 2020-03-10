package core

import core.interpreter.Interpreter
import core.util.Examples

case object Main {
  def main(args: Array[String]): Unit = println(Interpreter.explain(Examples.letrec))
}
