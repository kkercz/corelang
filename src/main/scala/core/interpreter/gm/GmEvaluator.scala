package core.interpreter.gm

import core.interpreter.gm.data.Instruction.Unwind
import core.interpreter.gm.data.{Instruction, Node, State}

case object GmEvaluator {

  def eval(state: State): List[State] =
    if (isFinal(state)) {
      List(state)
    } else {
      val rest = eval(prepare(nextState(state)))
      state :: rest
    }

  private def isFinal(state: State): Boolean = state.code.isEmpty

  private def prepare(state: State): State = state.withStats(state.stats.incrSteps())

  private def nextState(state: State): State = executeInstruction(state.code.head, state.withCode(state.code.tail))

  private def executeInstruction(instr: Instruction, state: State): State = instr match {
    case Instruction.PushGlobal(name) =>
      val globalAddress = state.globals.getOrElse(name, throw new IllegalArgumentException(s"Undeclared global: $name"))
      state.withStack(globalAddress :: state.stack)
    case Instruction.PushInt(num) =>
      state.allocAndPush(Node.Num(num))
    case Instruction.MkApp =>
      val (List(a1, a2), newStack) = state.stack.splitAt(2)
      state.withStack(newStack).allocAndPush(Node.Ap(a1, a2))
    case Instruction.Push(n) =>
      val arg = state.heap.lookup(state.stack(n + 1)).asInstanceOf[Node.Ap].a2
      state.withStack(arg :: state.stack)
    case Instruction.Slide(n) =>
      state.withStack(state.stack.head :: state.stack.tail.drop(n))
    case Instruction.Unwind => state.heap.lookup(state.stack.head) match {
      case Node.Num(_) => state
      case Node.Ap(a1, _) => state.withStack(a1 :: state.stack).withCode(Unwind :: state.code)
      case Node.Global(arity, code) =>
        if (state.stack.tail.length < arity)
          throw new IllegalArgumentException("Unwinding with too few arguments")
        else
          state.withCode(code)
    }
  }
}
