package core.interpreter.gm

import core.interpreter.data.{Address, Globals}
import core.interpreter.gm.data.Node.{Ap, Global, Num}
import core.interpreter.gm.data.{Instruction, State}
import core.util.PrintableText
import core.util.PrintableText.{Indented, Newline, Str, concat, fromString, interleave}

case object GmEvaluationPrettyPrinter {

  def prettyPrint(states: List[State]): String = {
    val statesWithPrevious: List[(Option[State], State)] = (None, states.head) :: (states map { Some(_) } zip states.drop(1))
    interleave(
      Newline ++ Newline,
      statesWithPrevious map { case (previous, state) => printState(previous, state) }
    ).printOut()
  }

  def printState(previousState: Option[State], state: State): PrintableText = {
    concat(
      "-" * 10, s" Step ${state.stats.steps} (reductions: ${state.stats.reductions}) ", "-" * 10, Newline,
      interleave(
        Newline ++ "      |" ++ Newline ++ "      |" ++ Newline,
        state.stack.reverse map { node => printSpineNode(node, state.heap, state.globals) }), Newline,
      printCode(state.code),
      Newline, Newline
    )
  }

  private def printSpineNode(address: Address, heap: GmHeap, globals: Globals): PrintableText = {

    def heapValue(a: Address, depth: Int = 2): PrintableText = {
      if (depth <= 0) {
        s"[$a]"
      } else {
        heap.lookup(address) match {
          case Num(int) => int.toString
          case Ap(a1, a2) => s"@[$a1] ---- " ++ heapValue(a2, depth - 1)
          case Global(_, code) => globalName(address, globals) ++ ": " ++ printCode(code)
        }
      }
    }

    Str(f"[$address%2d]: ") ++ Indented(heapValue(address))
  }

  def globalName(address: Address, globals: Globals): String = globals.find({ case (_, addr) => addr == address }).get._1

  def printCode(code: GmCode): String = code.map(printInstruction).mkString("[", ", ", "]")

  def printInstruction(inst: Instruction): String = inst match {
    case Instruction.Unwind => "Unwind"
    case Instruction.MkApp => "MkApp"
    case Instruction.PushGlobal(name) => "PushGlobal " + name
    case Instruction.PushInt(num) => "PushInt " + num
    case Instruction.Push(n) => "Push " + n
    case Instruction.Slide(n) => "Slide " + n
  }


}
