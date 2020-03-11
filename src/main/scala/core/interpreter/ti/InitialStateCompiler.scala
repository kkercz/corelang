package core.interpreter.ti

import core.ast.{CoreProgram, Name}
import core.interpreter.data.{Address, Globals, Heap, Node, State, Stats, TiHeap}
import core.lang.Prelude

case object InitialStateCompiler {
  def compile(program: CoreProgram): State = {
    val scs = program ++ Prelude.builtInFunctions
    val (initialHeap, globals): (TiHeap, Globals) =
      scs.foldLeft((Heap.empty[Node](), Map[Name, Address]()))({
        case ((heap, globals), sc) =>
          val (newHeap, scAddress) =
            heap.alloc(Node.SC(sc.name, sc.vars, sc.body))
          (newHeap, globals.updated(sc.name, scAddress))
      })

    val addressOfMain = globals.getOrElse("main", {
      throw new IllegalArgumentException("'main' supercombinator not found")
    })
    val initialStack = List(addressOfMain)
    val initialDump = List()

    State(initialStack, initialHeap, initialDump, globals, Stats())
  }
}
