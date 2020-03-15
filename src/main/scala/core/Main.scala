package core

import core.interpreter.Interpreter

case object Main {
  def main(args: Array[String]): Unit = println(Interpreter.explain(
    """
      |square x = K x x ;
      |main = square (square 3)
      |""".stripMargin))
}
