package core

import core.ast.prettyprint.ProgramPrettyPrinter
import core.interpreter.Interpreter
import core.interpreter.gm.GmEvaluationPrettyPrinter
import core.interpreter.gm.data.State
import core.interpreter.ti.TiEvaluationPrettyPrinter
import core.interpreter.ti.data.TiState
import core.parser.Parser
import core.util.Examples

case object Main {
  def main(args: Array[String]): Unit = explainGm(Examples.lazyExample)

  private def prettyPrint(program: String): Unit = println(ProgramPrettyPrinter.prettyPrint(Parser.parseCoreProgram(program)))

  private def explainTi(program: String): Unit = {
    val result: List[TiState] = Interpreter.Ti.eval(program)
    println(TiEvaluationPrettyPrinter.prettyPrint(result.drop(result.length - 2)))
  }

  private def explainGm(program: String): Unit = {
    val result: List[State] = Interpreter.Gm.eval(program)
    println(GmEvaluationPrettyPrinter.prettyPrint(result))
  }
}
