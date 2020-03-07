package com.kkercz.core.interpreter.data

import com.kkercz.core.interpreter.data

case class State(stack: Stack, heap: TiHeap, dump: Dump, globals: Globals, stats: Stats) {
  def withStack(stack: Stack): State = State(stack, heap, dump, globals, stats);
  def withHeap(heap: TiHeap): State = State(stack, heap, dump, globals, stats);
  def withDump(dump: Dump): State = State(stack, heap, dump, globals, stats);
  def withGlobals(globals: Globals): State = State(stack, heap, dump, globals, stats);
  def withStats(stats: Stats): State = State(stack, heap, dump, globals, stats);

}

object State {
  def empty(): State = data.State(List(), Heap(), List(), Map(), Stats())
}
