package core.interpreter.gm

import core.ast.{CoreProgram, CoreSc, Expr, Name}
import core.interpreter.gm.data.Instruction._
import core.interpreter.gm.data.{Node, State}
import core.lang.Prelude

case object GmCompiler {

  type Bindings = Map[Name, Int]

  def compile(program: CoreProgram): State = {
    val emptyState = State.empty()
    val allSupercombinators = Prelude.basicFunctions ++ program
    val (heap, globals) = allSupercombinators.foldLeft((emptyState.heap, emptyState.globals))({ case ((heap, globals), sc) =>
      val node = compileSC(sc)
      val (newHeap, newAddress) = heap.alloc(node)
      (newHeap, globals.updated(sc.name, newAddress))
    })

    emptyState
      .withGlobals(globals)
      .withHeap(heap)
      .withCode(List(PushGlobal("main"), Unwind))
  }

  def compileSC(sc: CoreSc): Node.Global = {
    val env: Bindings = Map(sc.vars.zip(List.range(0, sc.vars.length)): _*)
    Node.Global(sc.vars.length, compileR(sc.body, env))
  }

  def compileR(body: Expr[Name], env: Bindings): GmCode = compileC(body, env) ++ List(Slide(env.size + 1), Unwind)

  def compileC(body: Expr[Name], env: Bindings): GmCode = body match {
    case Expr.Var(name) => List(env.get(name) match {
      case Some(value) => Push(value)
      case None => PushGlobal(name)
    })
    case Expr.Num(value) => List(PushInt(value))
    case Expr.Ap(lhs, rhs) => compileC(rhs, env) ++ compileC(lhs, withOffset(env, 1)) ++ List(MkApp)
    case Expr.Constr(tag, arity) => ???
    case Expr.Let(isRec, definitions, body) => ???
    case Expr.Case(expr, alternatives) => ???
    case Expr.Lambda(variables, body) => ???
  }

  def withOffset(env: Bindings, offset: Int): Bindings = env.map({ case (k, v) => (k, v + offset) })

}
