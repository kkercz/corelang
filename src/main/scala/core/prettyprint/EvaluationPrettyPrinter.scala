package core.prettyprint

import core.interpreter.data.{Address, Globals, Heap, Node, Stack, State, TiHeap}
import core.util.PrintableText
import core.util.PrintableText.{Indented, Newline, Str, concat, fromString, interleave}

case object EvaluationPrettyPrinter {

  def prettyPrint(states: List[State]): String = {
    val statesWithPrevious: List[(Option[State], State)] = (None, states.head) :: (states map { Some(_) } zip states.drop(1))
    interleave(
      Newline ++ Newline,
      statesWithPrevious map { case (previous, state) => printState(previous, state) }
    ).printOut()
  }

  def printState(previousState: Option[State], state: State): PrintableText = {
    val previousHeap = previousState.map(s => s.heap).getOrElse(Heap.empty())
    val newHeapAddresses = state.heap.addresses().removedAll(previousHeap.addresses()).toList
    val allRelevantAddresses = allAddressesInUse(state.stack, state.heap).filter(a => !newHeapAddresses.contains(a))
    concat(
      "-" * 10, s" Step ${state.stats.steps} (reductions: ${state.stats.reductions}) ", "-" * 10, Newline,
      "Output: ", state.outputAsString(), Newline,
      "Dump: " ++ interleave(", ", state.dump.map(s => s.mkString("[", ", ", "]"))), Newline, Newline,
      interleave(
        Newline ++ "      |" ++ Newline ++ "      |" ++ Newline,
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
    allAddressesInUse(stack, heap, Set()).distinct
}

  private def printHeapEntries(heap: TiHeap, addresses: List[Address], name: String): PrintableText = {
    if (addresses.nonEmpty) concat(
        s"vvvvv $name vvvvv", Newline,
        interleave(Newline, addresses.sorted.map(a => heapEntry(a, heap.lookup(a)))))
    else ""
  }

  private def heapEntry(a: Address, node: Node): PrintableText = f"[$a%2d] = " ++ Indented(node match {
    case Node.App(a1, a2) => s"[$a1] [$a2]"
    case Node.Ref(a) => s"[$a*]"
    case Node.SC(name, bindings, body) =>s"$name" + " " + bindings.mkString(" ") + " = " ++ Indented(ProgramPrettyPrinter.ppr(body))
    case Node.Num(value) => value.toString
    case Node.Primitive(op) => s"[$a*]: ${op.symbol}"
    case c: Node.Constr => s"[$a*]: ${c.display()}"
    case Node.Case(expr, alternatives) => s"[$a] case $expr of $alternatives"
  })

  private def printSpineNode(address: Address, heap: TiHeap, globals: Globals): PrintableText = {

    val heapValue: PrintableText = heap.lookup(address) match {
      case Node.Ref(a) => s"[$a*]"
      case Node.App(a1, a2) => s"@[$a1] ---- " ++ heapEntry(a2, heap.lookup(a2))
      case Node.SC(name, bindings, body) => s"$name" + " " + bindings.mkString(" ") + " = " ++ Indented(ProgramPrettyPrinter.ppr(body))
      case Node.Num(value) => s"$value"
      case Node.Primitive(op) => op.symbol
      case c: Node.Constr => c.display()
      case Node.Case(expr, alternatives) => s"case [$expr] of ${alternatives}"
    }
    Str(f"[$address%2d]: ") ++ Indented(heapValue)
  }


}
