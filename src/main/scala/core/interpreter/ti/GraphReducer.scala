package core.interpreter.ti

import core.ast.{CoreExpr, Expr, Name}
import core.interpreter.data._
import core.interpreter.ti.data.{Node, TiState}
import core.interpreter.ti.gc.GarbageCollector

import scala.annotation.tailrec

case object GraphReducer {

  type Env = Map[Name, Address]


  def eval(state: TiState): List[TiState] = {
    @tailrec
    def eval(state: TiState, result: List[TiState]): List[TiState] = {
      if (isFinal(state)) {
        state :: result
      } else {
        val nextState = prepare(state, next(state))
        eval(nextState, state :: result)
      }
    }
    eval(state, List()).reverse
  }

  def prepare(previousState: TiState, state: TiState): TiState = {
    val newStats = state.stats.incrSteps()
    val maybePreviousStackHead = previousState.stack.headOption.map(previousState.heap.lookup)
    val reductionHappened = maybePreviousStackHead match {
      case Some(head) => (head match {
        case _: Node.SC => true
        case _: Node.Primitive => true
        case Node.Constr(e, args) if e.arity == args.length => true
        case _ => false }) && previousState.dump == state.dump
      case None => false
    }

    val newStats2 = if (reductionHappened) newStats.incrReductions() else newStats
    val newState = GarbageCollector.gc(state.withStats(newStats2))

    newState
  }

  private def isFinal(state: TiState): Boolean = state.stack match {
    case Nil if state.dump.isEmpty => true
    case addr :: Nil => state.heap.lookup(addr).isData && state.dump.isEmpty
    case _ => false
  }

  private def next(state: TiState): TiState = state.stack match {
    case addr :: tail =>
      state.heap.lookup(addr) match {
        case Node.Ref(a) => state.withStack(a :: tail)
        case Node.Constr(expr, args) if args.isEmpty && expr.arity > 0 =>
          val appliedArgs = state.applicationArguments(expr.arity, expr.syntax())
          state.updateRedexRoot(expr.arity, Node.Constr(expr, appliedArgs))
        case data if data.isData && state.dump.nonEmpty => state.popDump()
        case data if data.isData => throw new IllegalArgumentException(s"${data.display()} is not a function!")
        case Node.App(a1, _) => state.withStack(a1 :: addr :: tail)
        case Node.SC(name, arguments, body, closure) =>
          val env = state.globals ++ closure ++ arguments.zip(state.applicationArguments(arguments.length, name))
          val (newHeap, newAddress) = instantiate(state.heap, env, body)
          state.withHeap(newHeap).updateRedexRoot(arguments.length, Node.Ref(newAddress))
        case Node.Primitive(op) =>
          val argAddresses = state.applicationArguments(op.arity, op.symbol)
          val argNodes = argAddresses.map(state.heapValue)
          val nonAtomicArg = argAddresses.zip(argNodes).take(op.evaluatedArgumentsExpected).find(a => !a._2.isData)
          nonAtomicArg match {
            case Some((addr, _)) => state.dumpCurrentStack(addr)
            case None =>
              val (newState, newNode) = op.apply(state, argNodes)
              newNode match {
                case Some(value) => newState.updateRedexRoot(op.arity, value)
                case None => newState.withStack(tail.drop(op.arity))
              }
          }
        case Node.Case(expr, alternatives) => state.heapValue(expr) match {
            case constr: Node.Constr =>
              val selectedCase = alternatives.find(p => p.expr.tag == constr.expr.tag).getOrElse(throw new IllegalArgumentException(s"No case for tag ${constr.expr.tag}"))
              val newEnv = selectedCase.env ++ Map(selectedCase.expr.args.zip(constr.args): _*)
              val (newHeap, newAddr) = instantiate(state.heap, newEnv, selectedCase.expr.body)
              state.withStack(newAddr :: tail).withHeap(newHeap.update(addr, Node.Ref(newAddr)))
            case _ => state.dumpCurrentStack(expr)
          }
      }
    case Nil if state.dump.nonEmpty => state.popDump()
    case Nil => throw new IllegalStateException("Stack should not be null if we want to compute next state")
  }

  private def instantiate(heap: TiHeap, env: Env, body: CoreExpr, resultAddress: Option[Address] = None): (TiHeap, Address) =
    body match {
      case Expr.Num(value) => heap.alloc(Node.Num(value), resultAddress)
      case Expr.Var(name) =>
        val varAddress = env(name)
        resultAddress match {
          case Some(_) => heap.alloc(Node.Ref(varAddress), resultAddress)
          case None => (heap, varAddress)
        }
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
          instantiate(accHeap, envWithDefs, defn._2, Some(envWithDefs(defn._1)))._1)

        instantiate(finalHeap, envWithDefs, body)
      case c: Expr.Constr[Name] => heap.alloc(Node.Constr(c.asInstanceOf[Expr.Constr[Name]]), resultAddress)
      case Expr.Case(expr, alternatives) =>
        val (newHeap, exprAddr) = instantiate(heap, env, expr)
        newHeap.alloc(Node.Case(exprAddr, alternatives.map(a => Node.Alternative(a, env))), resultAddress)
      case lambda: Expr.Lambda[Name] => heap.alloc(Node.SC("λ", lambda.variables, lambda.body, env), resultAddress)
    }
}
