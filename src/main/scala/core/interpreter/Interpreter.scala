package core.interpreter

import core.interpreter.data.{Node, State}
import core.interpreter.ti.GraphReducer.eval
import core.interpreter.ti.InitialStateCompiler.compile
import core.parser.Parser.parseCoreProgram

case object Interpreter {

  def runTemplateInstantiation(program: String): List[State] = eval(compile(parseCoreProgram(program)))

  def compute(program: String): String = showResult(runTemplateInstantiation(program))

  private def showResult(states: List[State]): String = {
    val finalState = states.last
    finalState.heap.lookup(finalState.stack.head) match {
      case Node.Num(value) => value.toString
      case node => node.toString
    }
  }
}

