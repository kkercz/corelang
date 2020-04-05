package core

import core.interpreter.Interpreter
import core.interpreter.ti.EvaluationPrettyPrinter
import core.interpreter.ti.data.TiState
import core.parser.{Parser, ProgramPrettyPrinter}
import core.util.Examples

case object Main {
  def main(args: Array[String]): Unit = explain(Examples.sieve)

  private def prettyPrint(program: String) = println(ProgramPrettyPrinter.prettyPrint(Parser.parseCoreProgram(program)))

  private def explain(program: String) = {
    val result: List[TiState] = Interpreter.Ti.eval(program)
    println(EvaluationPrettyPrinter.prettyPrint(result.drop(result.length - 2)))
  }
}
