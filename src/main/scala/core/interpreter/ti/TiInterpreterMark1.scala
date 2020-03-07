package core.interpreter.ti

import core.ast.{CoreExpr, CoreProgram, Expr, Name}
import core.interpreter.Interpreter
import core.interpreter.data._
import core.lang.Prelude
import core.parser.Parser.parseCoreProgram

case object TiInterpreterMark1 extends Interpreter {

  def run(program: String): String =
    show(eval(compile(parseCoreProgram(program))))

  def show(states: List[State]): String = {
    val finalState = states.last
    finalState.heap.lookup(finalState.stack.head) match {
      case Node.Num(value) => value.toString
      case node => node.toString
    }
  }

  def compile(program: CoreProgram): State = {
    val scs = program ++ Prelude.builtInFunctions
    val (initialHeap, globals): (TiHeap, Globals) =
      scs.foldLeft((Heap[Node](), Map[Name, Address]()))({
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

  def eval(state: State): List[State] = {
    if (isFinal(state)) {
      List(state)
    } else {
      state :: eval(next(state).withStats(state.stats.incrSteps()))
    }
  }

  def isFinal(state: State): Boolean = state.stack match {
    case addr :: Nil => state.heap.lookup(addr).isData
    case _ => false
  }

  def next(state: State): State = state.stack match {
    case addr :: tail =>
      state.heap.lookup(addr) match {
        case Node.Num(_)      => throw new IllegalArgumentException("Number is not a function!")
        case Node.App(a1, _) => state.withStack(a1 :: addr :: tail)
        case Node.SC(name, bindings, body) =>
          val (args, rest) = tail splitAt bindings.length
          if (args.length != bindings.length)
            throw new IllegalArgumentException("Too few arguments provided to function " + name)

          val env = Map(bindings zip argAddresses(args, state.heap):_* ) ++ state.globals
          val (newHeap, newAddress) = instantiate(state.heap, env, body)
          val newState = state.withStack(newAddress :: rest).withHeap(newHeap)
          newState
      }
    case Nil => throw new IllegalStateException("Stack should never be null")
  }

  def argAddresses(stack: Stack, heap: TiHeap): List[Address] = stack.map( addr => heap.lookup(addr) match {
      case Node.App(_, arg) => arg
      case _ => throw new IllegalStateException("Expected to see function application here")
    })

  def instantiate(heap: TiHeap, env: Map[Name, Address], body: CoreExpr): (TiHeap, Address) =
    body match {
      case Expr.Num(value)                    => heap.alloc(Node.Num(value))
      case Expr.Var(name)                     => (heap, env.getOrThrow(name))
      case Expr.Ap(lhs, rhs)                  =>
        val (heap1, addr1) = instantiate(heap, env, lhs)
        val (heap2, addr2) = instantiate(heap1, env, rhs)
        heap2.alloc(Node.App(addr1, addr2))
      case Expr.Constr(tag, arity)            => ???
      case Expr.Let(isRec, definitions, body) => ???
      case Expr.Case(expr, alternatives)      => ???
      case Expr.Lambda(variables, body)       => ???
    }

}
