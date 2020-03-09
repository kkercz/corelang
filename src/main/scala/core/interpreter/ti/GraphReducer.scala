package core.interpreter.ti

import core.ast.{CoreExpr, Expr, Name}
import core.interpreter.data._

case object GraphReducer {

  def eval(state: State): List[State] = {
    if (isFinal(state)) {
      List(state)
    } else {
      state :: eval(next(state).withStats(state.stats.incrSteps()))
    }
  }

  private def isFinal(state: State): Boolean = state.stack match {
    case addr :: Nil => state.heap.lookup(addr).isData
    case _ => false
  }

  private def next(state: State): State = state.stack match {
    case addr :: tail =>
      state.heap.lookup(addr) match {
        case Node.Num(_)      => throw new IllegalArgumentException("Number is not a function!")
        case Node.App(a1, _) => state.withStack(a1 :: addr :: tail)
        case Node.SC(name, arguments, body) =>
          val (args, rest) = tail splitAt arguments.length
          val env = argBindings(name, arguments, args, state.heap) ++ state.globals
          val (newHeap, newAddress) = instantiate(state.heap, env, body)

          state.withHeap(newHeap).withStack(newAddress :: rest)
      }
    case Nil => throw new IllegalStateException("Stack should not be null if we want to compute next state")
  }

  private def argBindings(fun: Name, argNames: List[Name], stack: Stack, heap: TiHeap): Map[Name, Address] = {
    if (argNames.length != stack.length)
      throw new IllegalArgumentException("Too few arguments provided to function " + fun)

    val addresses: List[Address] = stack.map(addr => heap.lookup(addr) match {
      case Node.App(_, arg) => arg
      case _ => throw new IllegalStateException("Expected to see function application here")
    })

    Map( argNames zip addresses :_* )
  }

  private def instantiate(heap: TiHeap, env: Map[Name, Address], body: CoreExpr): (TiHeap, Address) =
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
