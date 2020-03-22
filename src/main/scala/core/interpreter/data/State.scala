package core.interpreter.data

import core.interpreter.data

case class State(stack: Stack, heap: TiHeap, dump: Dump, globals: Globals, stats: Stats) {
  def withStack(stack: Stack): State = State(stack, heap, dump, globals, stats);
  def withHeap(heap: TiHeap): State = State(stack, heap, dump, globals, stats);
  def withDump(dump: Dump): State = State(stack, heap, dump, globals, stats);
  def withGlobals(globals: Globals): State = State(stack, heap, dump, globals, stats);
  def withStats(stats: Stats): State = State(stack, heap, dump, globals, stats);

  def result: Node = heap.lookup(stack.last)

}

object State {
  def empty(): State = data.State(List(), Heap.empty(), List(), Map(), Stats())
}
