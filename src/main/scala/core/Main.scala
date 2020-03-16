package core

import core.interpreter.Interpreter

case object Main {
  def main(args: Array[String]): Unit = println(Interpreter.explain(
    """main = let id1 = I I I
      |in id1 id1 3""".stripMargin))
}
