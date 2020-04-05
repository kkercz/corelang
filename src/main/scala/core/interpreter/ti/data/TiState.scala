package core.interpreter.ti.data

import core.interpreter.data._
import core.interpreter.ti
import core.interpreter.ti.TiHeap

import scala.annotation.tailrec

case class TiState(stack: Stack, heap: TiHeap, dump: Dump, globals: Globals, stats: Stats, output: List[Node]) {

  def appendToOutput(node: Node): TiState = TiState(stack, heap, dump, globals, stats, node :: output)
  def outputAsString(): String = output.reverse.map(n => n.display()).mkString(" ")

  def applicationArguments(arity: Int, functionName: String = "???"): List[Address] =
    if (stack.tail.length < arity)
      throw new IllegalArgumentException(s"Too few arguments provided to function $functionName")
    else
      stack.slice(1, arity + 1).map(addr => heap.lookup(addr) match {
        case Node.App(_, arg) => arg
        case _ => throw new IllegalStateException("Expected to see function application here")
    })

  def updateRedexRoot(arity: Int, newNode: Node): TiState = {
    val newStack = stack.splitAt(arity)._2
    withStack(newStack).withHeap(heap.update(newStack.head, newNode))
  }

  def popDump(): TiState = withStack(dump.head).withDump(dump.tail)
  def dumpCurrentStack(newStackHead: Address): TiState = withStack(newStackHead :: Nil).withDump(stack :: dump)

  @tailrec
  final def heapValue(addr: Address): Node = heap.lookup(addr) match {
    case Node.Ref(a) => heapValue(a)
    case n => n
  }

  def withStack(stack: Stack): TiState = TiState(stack, heap, dump, globals, stats, output);
  def withDump(dump: Dump): TiState = TiState(stack, heap, dump, globals, stats, output);
  def withHeap(heap: TiHeap): TiState = TiState(stack, heap, dump, globals, stats, output);
  def withGlobals(globals: Globals): TiState = TiState(stack, heap, dump, globals, stats, output);
  def withStats(stats: Stats): TiState = TiState(stack, heap, dump, globals, stats, output);

  def result: Node = heap.lookup(stack.last)

}

object TiState {
  def empty(): TiState = ti.data.TiState(List(), Heap.empty(), List(), Map(), Stats(), Nil)
}
