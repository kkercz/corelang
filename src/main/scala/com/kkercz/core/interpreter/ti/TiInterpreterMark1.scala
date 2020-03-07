package com.kkercz.core.interpreter.ti

import com.kkercz.core.ast.{CoreProgram, Name}
import com.kkercz.core.interpreter.Interpreter
import com.kkercz.core.interpreter.data._
import com.kkercz.core.lang.Prelude
import com.kkercz.core.parser.Parser.parseCoreProgram

case object TiInterpreterMark1 extends Interpreter {

  def run(program: String): String = show(reduce(compile(parseCoreProgram(program))))

  def compile(program: CoreProgram): State = {
    val scs = program ++ Prelude.builtInFunctions
    val (initialHeap, globals): (TiHeap, Globals) = scs.foldLeft((Heap[Node](), Map[Name, Address]()))( { case ((heap, globals), sc) =>
      val (newHeap, scAddress) = heap.alloc(Node.SC(sc.name, sc.vars, sc.body))
      (newHeap, globals.updated(sc.name, scAddress))
    })

    val addressOfMain = globals.getOrElse("main", { throw new IllegalArgumentException("'main' supercombinator not found")})
    val initialStack = List(addressOfMain)
    val initialDump = List()

    State(initialStack, initialHeap, initialDump, globals, Stats())
  }

  def show(states: List[State]): String = {
    val finalState = states.head
    finalState.heap.lookup(finalState.stack.head).toString
  }

  def reduce(state: State): List[State] = ???

}
