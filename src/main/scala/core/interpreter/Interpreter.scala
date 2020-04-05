package core.interpreter

import core.ast.CoreProgram
import core.interpreter.gm.data.State
import core.interpreter.gm.{GmCompiler, GmEvaluator}
import core.interpreter.ti.data.TiState
import core.interpreter.ti.{GraphReducer, InitialStateCompiler}
import core.parser.Parser.parseCoreProgram

trait Interpreter[State] {

  def compute: String => String = show.compose(eval)

  def eval: String => List[State] = run.compose(parseCoreProgram)

  protected def run: CoreProgram => List[State]

  protected def show: List[State] => String
}

object Interpreter {

  /**
   * The template instantiation interpreter
   */
  case object Ti extends Interpreter[TiState] {
    override def run: CoreProgram => List[TiState] = p => GraphReducer.eval(InitialStateCompiler.compile(p))

    override def show: List[TiState] => String = states => {
      val finalState = states.last
      if (finalState.stack.isEmpty)
        finalState.outputAsString()
      else
        finalState.heap.lookup(finalState.stack.head).display()
    }
  }

  /**
   * The G-machine interpreter
   */
  case object Gm extends Interpreter[State] {
    override def run: CoreProgram => List[State] = p => GmEvaluator.eval(GmCompiler.compile(p))

    override def show: List[State] => String = states => {
      val finalState = states.last
      finalState.heap.lookup(finalState.stack.head).display()
    }
  }
}
