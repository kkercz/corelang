package core.interpreter.ti

import core.ast.{CoreProgram, CoreSc, Name}
import core.interpreter.data.{Address, Globals, Heap, Node, State, Stats, TiHeap}
import core.lang.Prelude

case object InitialStateCompiler {
  def compile(program: CoreProgram): State = {
    val scs = program ++ Prelude.builtInFunctions
    val (heapWithSupercombinators, globalSupercombinators): (TiHeap, Globals) = initGlobalNames[CoreSc](scs, sc => (sc.name, Node.SC(sc.name, sc.vars, sc.body)))
    val (heapWithPrimitives, globalPrimitives): (TiHeap, Globals) = initGlobalNames[ArithmeticOperation](ArithmeticOperation.all, p => (p.symbol, Node.Primitive(p)))

    val initialHeap = heapWithSupercombinators ++ heapWithPrimitives
    val initialGlobals = globalSupercombinators ++ globalPrimitives

    val addressOfMain = initialGlobals.getOrElse("main", {
      throw new IllegalArgumentException("'main' supercombinator not found")
    })
    val initialStack = List(addressOfMain)
    val initialDump = List()

    State(initialStack, initialHeap, initialDump, initialGlobals, Stats())
  }

  def initGlobalNames[T](items: List[T], mapping: T => (Name, Node)): (TiHeap, Globals) = items.foldLeft((Heap.empty[Node](), Map[Name, Address]()))({
    case ((heap, globals), item) =>
      val (name, node) = mapping(item)
      val (newHeap, scAddress) = heap.alloc(node)
      (newHeap, globals.updated(name, scAddress))
  })
}
