package core

import core.interpreter.Interpreter.runTemplateInstantiation
import core.parser.Parser
import core.prettyprint.{EvaluationPrettyPrinter, ProgramPrettyPrinter}

case object Main {
  def main(args: Array[String]): Unit = explain("main = printList [1,2,3]")

  private def prettyPrint(program: String) = println(ProgramPrettyPrinter.prettyPrint(Parser.parseCoreProgram(program)))

  private def explain(program: String) = println(EvaluationPrettyPrinter.prettyPrint(runTemplateInstantiation(program)))
}
