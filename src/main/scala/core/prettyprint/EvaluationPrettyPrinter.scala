package core.prettyprint

import core.interpreter.data.{Address, Globals, Heap, Node, Stack, State, TiHeap}
import core.util.PrintableText
import core.util.PrintableText.{Indented, Newline, Str, concat, fromString, interleave}

case object EvaluationPrettyPrinter {

  def prettyPrint(states: List[State]): PrintableText = {
    val statesWithPrevious: List[(Option[State], State)] = (None, states.head) :: (states map { Some(_) } zip states.drop(1))
    interleave(
      Newline ++ Newline,
      statesWithPrevious map { case (previous, state) => printState(previous, state) }
    )
  }

  def printState(previousState: Option[State], state: State): PrintableText = {
    val previousHeap = previousState.map(s => s.heap).getOrElse(Heap())
    val newHeapAddresses = state.heap.addresses().removedAll(previousHeap.addresses()).toList
    val allRelevantAddresses = allAddressesInUse(state.stack, state.heap).filter(a => !newHeapAddresses.contains(a))
    concat(
      "-" * 10, s" Step ${state.stats.steps} ", "-" * 10, Newline, Newline,
      interleave(
        Newline ++ "       |" ++ Newline ++ "       V" ++ Newline,
        state.stack.reverse map { node => printSpineNode(node, state.heap, state.globals) }), Newline, Newline,
      printHeapEntries(state.heap, newHeapAddresses, "new heap entries"), Newline, Newline,
      printHeapEntries(state.heap, allRelevantAddresses, "relevant heap entries")
    )
  }

  private def allAddressesInUse(stack: Stack, heap: TiHeap): List[Address] = {
    def allAddressesInUse(stack: Stack, heap: TiHeap, seen: Set[Address]): List[Address] = {
      stack match {
        case head :: tail if !seen.contains(head) =>
          heap.lookup(head) match {
            case Node.App(a1, a2) => a1 :: a2 :: allAddressesInUse(a1 :: a2 :: tail, heap, seen.incl(head))
            case Node.Ref(a) => a :: allAddressesInUse(a :: tail, heap, seen.incl(head))
            case _ => allAddressesInUse(tail, heap, seen)
          }
        case _ :: tail => allAddressesInUse(tail, heap, seen)
        case Nil => List()
      }
    }
    allAddressesInUse(stack, heap, Set())
}

  private def printHeapEntries(heap: TiHeap, addresses: List[Address], name: String): PrintableText = {
    if (addresses.nonEmpty) concat(
        s"vvvvv $name vvvvv", Newline,
        interleave(Newline, addresses.sorted.map(a => f"[$a%2d] = " ++ Indented(heapEntry(heap.lookup(a))))))
    else ""
  }

  private def heapEntry(node: Node): PrintableText = node match {
    case Node.App(a1, a2) => s"[$a1] [$a2]"
    case Node.SC(name, bindings, body) =>s"$name" + " " + bindings.mkString(" ") + " = " ++ Indented(ProgramPrettyPrinter.prettyPrint(body))
    case Node.Num(value) => value.toString
  }

  private def printSpineNode(address: Address, heap: TiHeap, globals: Globals): PrintableText = {

    val heapValue: PrintableText = heap.lookup(address) match {
      case Node.App(a1, a2) => s"[$a1] ----> [$a2]"
      case Node.SC(name, bindings, body) => s"$name" + " " + bindings.mkString(" ") + " = " ++ Indented(ProgramPrettyPrinter.prettyPrint(body))
      case Node.Num(value) => s"$value"
    }
    Str(f"[$address%2d]: ") ++ Indented(heapValue)
  }


}
