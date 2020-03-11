package core.interpreter.ti

import core.ast.{CoreExpr, Expr, Name}
import core.interpreter.data._

case object GraphReducer {

  type Env = Map[Name, Address]

  def eval(state: State): List[State] = {
    if (isFinal(state)) {
      List(state)
    } else {
      state :: eval(prepare(state, next(state)))
    }
  }

  def prepare(previousState: State, state: State): State = {
    state.withStats(state.stats.incrSteps())
  }

  private def isFinal(state: State): Boolean = state.stack match {
    case addr :: Nil => state.heap.lookup(addr).isData
    case _ => false
  }

  private def next(state: State): State = state.stack match {
    case addr :: tail =>
      state.heap.lookup(addr) match {
        case Node.Ref(a) => state.withStack(a :: tail)
        case Node.Num(_) => throw new IllegalArgumentException("Number is not a function!")
        case Node.App(a1, _) => state.withStack(a1 :: addr :: tail)
        case Node.SC(name, arguments, body) =>
          val (args, rest) = tail splitAt arguments.length
          val env = state.globals ++ argBindings(name, arguments, args, state.heap)
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

    Map(argNames zip addresses: _*)
  }

  private def instantiate(heap: TiHeap, env: Env, body: CoreExpr, resultAddress: Option[Address] = None): (TiHeap, Address) =
    body match {
      case Expr.Num(value) => heap.alloc(Node.Num(value), resultAddress)
      case Expr.Var(name) =>
        val varAddress = env.getOrThrow(name)
        resultAddress.fold((heap, varAddress))(_ => heap.alloc(Node.Ref(varAddress), resultAddress))
      case Expr.Ap(lhs, rhs) =>
        val (heap1, addr1) = instantiate(heap, env, lhs)
        val (heap2, addr2) = instantiate(heap1, env, rhs)
        heap2.alloc(Node.App(addr1, addr2), resultAddress)
      case Expr.Let(_, definitions, body) =>
        val (heapWithDefs, envWithDefs): (TiHeap, Env) = definitions.foldLeft(heap, env)((acc, defn) => {
          val (newHeap, reservedAddress) = acc._1.reserve()
          (newHeap, acc._2.updated(defn._1, reservedAddress))
        })

        val finalHeap: TiHeap = definitions.foldLeft(heapWithDefs)((accHeap, defn) =>
          instantiate(accHeap, envWithDefs, defn._2, Some(envWithDefs.getOrThrow(defn._1)))._1)

        instantiate(finalHeap, envWithDefs, body)
      case Expr.Constr(tag, arity) => ???
      case Expr.Case(expr, alternatives) => ???
      case Expr.Lambda(variables, body) => ???
    }
}
