package core.interpreter

import core.interpreter.data.State
import core.interpreter.ti.GraphReducer.eval
import core.interpreter.ti.InitialStateCompiler.compile
import core.parser.Parser.parseCoreProgram

case object Interpreter {

  def runTemplateInstantiation(program: String): List[State] = eval(compile(parseCoreProgram(program)))

  def compute(program: String): String = showResult(runTemplateInstantiation(program))

  private def showResult(states: List[State]): String = {
    val finalState = states.last
    if (finalState.stack.isEmpty)
      finalState.outputAsString()
    else
      finalState.heap.lookup(finalState.stack.head).display()
  }
}

