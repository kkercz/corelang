package core.interpreter.data

import core.interpreter.data

import scala.annotation.tailrec

case class State(stack: Stack, heap: TiHeap, dump: Dump, globals: Globals, stats: Stats) {

  def applicationArguments(arity: Int, functionName: String = "???"): List[Address] =
    if (stack.tail.length < arity)
      throw new IllegalArgumentException(s"Too few arguments provided to function $functionName")
    else
      stack.slice(1, arity + 1).map(addr => heap.lookup(addr) match {
        case Node.App(_, arg) => arg
        case _ => throw new IllegalStateException("Expected to see function application here")
    })

  def updateRedexRoot(arity: Int, newNode: Node): State = {
    val newStack = stack.splitAt(arity)._2
    withStack(newStack).withHeap(heap.update(newStack.head, newNode))
  }

  def popDump(): State = withStack(dump.head).withDump(dump.tail)
  def dumpCurrentStack(newStackHead: Address): State = withStack(newStackHead :: Nil).withDump(stack :: dump)

  @tailrec
  final def heapValue(addr: Address): Node = heap.lookup(addr) match {
    case Node.Ref(a) => heapValue(a)
    case n => n
  }

  def withStack(stack: Stack): State = State(stack, heap, dump, globals, stats);
  def withHeap(heap: TiHeap): State = State(stack, heap, dump, globals, stats);
  def withGlobals(globals: Globals): State = State(stack, heap, dump, globals, stats);
  def withStats(stats: Stats): State = State(stack, heap, dump, globals, stats);

  private def withDump(dump: Dump): State = State(stack, heap, dump, globals, stats);

  def result: Node = heap.lookup(stack.last)

}

object State {
  def empty(): State = data.State(List(), Heap.empty(), List(), Map(), Stats())
}
