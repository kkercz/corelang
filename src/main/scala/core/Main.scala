package core

import core.interpreter.Interpreter

case object Main {
  def main(args: Array[String]): Unit = println(Interpreter.explain("main = S K K 3"))
}
