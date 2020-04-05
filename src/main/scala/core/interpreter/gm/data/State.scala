package core.interpreter.gm.data

import core.interpreter.data.{Globals, Heap, Stack, Stats}
import core.interpreter.gm.{GmCode, GmHeap}

case class State(code: GmCode, stack: Stack, heap: GmHeap, globals: Globals, stats: Stats) {
  def withCode(code: GmCode): State = State(code, stack, heap, globals, stats)
  def withStack(stack: Stack): State = State(code, stack, heap, globals, stats)
  def withHeap(heap: GmHeap): State = State(code, stack, heap, globals, stats)
  def withGlobals(globals: Globals): State = State(code, stack, heap, globals, stats)
  def withStats(stats: Stats): State = State(code, stack, heap, globals, stats)

  def allocAndPush(node: Node): State = {
    val (newHeap, addr) = heap.alloc(node)
    withStack(addr :: stack).withHeap(newHeap)
  }
}

object State {
  def empty(): State = State(List(), List(), Heap.empty(), Map(), Stats())
}
