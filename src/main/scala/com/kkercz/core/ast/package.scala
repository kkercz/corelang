package com.kkercz.core

import com.kkercz.core.ast.Expr.Alter

package object ast {

  case class Supercombinator[T](name: Name, vars: List[T], body: Expr[T])

  type Program[T] = List[Supercombinator[T]]
  type CoreProgram = Program[Name]
  type CoreSc = Supercombinator[Name]

  type Name = String

  type CoreExpr = Expr[Name]
  type CoreAlter = Alter[Name]
}
