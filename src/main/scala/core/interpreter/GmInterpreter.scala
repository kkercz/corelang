package core.interpreter

import core.interpreter.gm.GmCompiler.compile
import core.interpreter.gm.GmEvaluator.eval
import core.interpreter.gm.data.State
import core.parser.Parser.parseCoreProgram


case object GmInterpreter {

  def runGMachine(program: String): List[State] = eval(compile(parseCoreProgram(program)))

  def compute(program: String): String = showResult(runGMachine(program))

  private def showResult(states: List[State]): String = {
    val finalState = states.last
    finalState.heap.lookup(finalState.stack.head).display()
  }
}

