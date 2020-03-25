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
    val newStats = state.stats.incrSteps()
    val previousStackHead = previousState.heap.lookup(previousState.stack.head)
    val reductionHappened = (previousStackHead.isInstanceOf[Node.SC] || previousStackHead.isInstanceOf[Node.Primitive]) &&
                            previousState.dump == state.dump

    val newStats2 = if (reductionHappened) newStats.incrReductions() else newStats
    state.withStats(newStats2)
  }

  private def isFinal(state: State): Boolean = state.stack match {
    case addr :: Nil => state.heap.lookup(addr).isData && state.dump.isEmpty
    case _ => false
  }

  private def next(state: State): State = state.stack match {
    case addr :: tail =>
      state.heap.lookup(addr) match {
        case Node.Ref(a) => state.withStack(a :: tail)
        case Node.Constr(expr, args) if args.isEmpty && expr.arity > 0 =>
          val appliedArgs = arguments(state.stack.slice(1, expr.arity + 1), state.heap)
          val newNode = Node.Constr(expr, appliedArgs)
          val rootStack = state.stack.drop(expr.arity)
          val newHeap = state.heap.update(rootStack.head, newNode)
          state.withStack(rootStack).withHeap(newHeap)
        case data if data.isData && state.dump.nonEmpty =>
            state.withStack(state.dump.head).withDump(state.dump.tail)
        case data if data.isData => throw new IllegalArgumentException(s"${data.display()} is not a function!")
        case Node.App(a1, _) => state.withStack(a1 :: addr :: tail)
        case Node.SC(name, arguments, body) =>
          val (args, rest) = tail.splitAt(arguments.length)
          val env = state.globals ++ argBindings(name, arguments, args, state.heap)
          val (newHeap, newAddress) = instantiate(state.heap, env, body)
          val redexRoot = if (args.isEmpty) addr else args.last
          val updatedHeap = newHeap.update(redexRoot, Node.Ref(newAddress))
          state.withHeap(updatedHeap).withStack(newAddress :: rest)
        case Node.Primitive(op) =>
          val (args, rest) = tail.splitAt(op.arity)
          val argNode = args
                        .map(a => state.heap.lookup(a).asInstanceOf[Node.App])
                        .map(app => (app.a2, deref(state.heap, state.heap.lookup(app.a2))))
          val nonAtomicArg = argNode.find(a => !a._2.isData)
          nonAtomicArg match {
            case Some((addr, _)) =>
              state.withStack(addr :: Nil).withDump(state.stack :: state.dump)
            case None =>
              val redexRoot = args.last
              val result = op.apply(argNode.map(a => a._2))
              val newHeap = state.heap.update(redexRoot, result)
              state.withStack(redexRoot :: rest).withHeap(newHeap)
          }
        case Node.Case(expr, alternatives) => deref(state.heap, state.heap.lookup(expr)) match {
            case constr: Node.Constr =>
              val selectedCase = alternatives.find(p => p.tag == constr.expr.tag).getOrElse(throw new IllegalArgumentException(s"No case for tag ${constr.expr.tag}"))
              val (newHeap, newAddr) = selectedCase.argsToExpression(state. heap, state.globals, constr.args)
              state.withStack(newAddr :: tail).withHeap(newHeap.update(addr, Node.Ref(newAddr)))
            case _ => state.withDump(state.stack :: state.dump).withStack(expr :: Nil)
          }
      }
    case Nil => throw new IllegalStateException("Stack should not be null if we want to compute next state")
  }

  @scala.annotation.tailrec
  private def deref(heap: TiHeap, n: Node): Node = n match {
    case Node.Ref(a) => deref(heap, heap.lookup(a))
    case _ => n
  }

  private def argBindings(fun: Name, argNames: List[Name], applicationNodes: List[Address], heap: TiHeap): Map[Name, Address] = {
    if (argNames.length != applicationNodes.length)
      throw new IllegalArgumentException("Too few arguments provided to function " + fun)

    Map(argNames zip arguments(applicationNodes, heap): _*)
  }

  def arguments(applicationNodes: List[Address], heap: TiHeap): List[Address] = applicationNodes.map(addr => heap.lookup(addr) match {
    case Node.App(_, arg) => arg
    case _ => throw new IllegalStateException("Expected to see function application here")
  })

  private def instantiate(heap: TiHeap, env: Env, body: CoreExpr, resultAddress: Option[Address] = None): (TiHeap, Address) =
    body match {
      case Expr.Num(value) => heap.alloc(Node.Num(value), resultAddress)
      case Expr.Var(name) =>
        val varAddress = env.getOrThrow(name)
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
          instantiate(accHeap, envWithDefs, defn._2, Some(envWithDefs.getOrThrow(defn._1)))._1)

        instantiate(finalHeap, envWithDefs, body)
      case c: Expr.Constr[Name] => heap.alloc(Node.Constr(c.asInstanceOf[Expr.Constr[Name]]))
      case Expr.Case(expr, alternatives) =>
        val (newHeap, exprAddr) = instantiate(heap, env, expr)
        newHeap.alloc(Node.Case(
          exprAddr,
          alternatives.map(a => Node.Alternative(a.tag, (heap: TiHeap, globals: Env, args: List[Address]) => {
            val newEnv = globals ++ env ++ Map(a.args.zip(args): _*)
            instantiate(heap, newEnv, a.body)
          }))
        ))
      case Expr.Lambda(variables, body) => ???
    }
}
