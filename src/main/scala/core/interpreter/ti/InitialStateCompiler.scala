package core.interpreter.ti

import core.ast.{CoreProgram, CoreSc, Name}
import core.interpreter.data.{Address, Globals, Heap, Stats}
import core.interpreter.ti.data.{Node, TiState}
import core.lang.Prelude

case object InitialStateCompiler {
  def compile(program: CoreProgram): TiState = {
    val scs = Prelude.basicFunctions ++ Prelude.structuredData ++ Prelude.lists ++ program
    val (heapWithSupercombinators, globalSupercombinators): (TiHeap, Globals) = initGlobalNames[CoreSc](Heap.empty())(scs, sc => (sc.name, Node.SC(sc.name, sc.vars, sc.body)))
    val (initialHeap, globalPrimitives): (TiHeap, Globals) = initGlobalNames[BuiltInFunction](heapWithSupercombinators)(BuiltInFunction.all, p => (p.symbol, Node.Primitive(p)))

    val initialGlobals = globalSupercombinators ++ globalPrimitives

    val addressOfMain = initialGlobals.getOrElse("main", {
      throw new IllegalArgumentException("'main' supercombinator not found")
    })
    val initialStack = List(addressOfMain)
    val initialDump = List()

    data.TiState(initialStack, initialHeap, initialDump, initialGlobals, Stats(), List())
  }

  def initGlobalNames[T](initialHeap: TiHeap)(items: List[T], mapping: T => (Name, Node)): (TiHeap, Globals) = items.foldLeft((initialHeap, Map[Name, Address]()))({
    case ((heap, globals), item) =>
      val (name, node) = mapping(item)
      val (newHeap, scAddress) = heap.alloc(node)
      (newHeap, globals.updated(name, scAddress))
  })
}
