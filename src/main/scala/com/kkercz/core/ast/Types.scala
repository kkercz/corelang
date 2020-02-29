package com.kkercz.core.ast

case object Types {
  type Program[T] = List[ScDefn[T]]
  type CoreProgram = Program[Name]
  type ScDefn[T] = (Name, List[T], Expr[T])
  type CoreScDefn = ScDefn[Name]

  type Name = String
  type CoreExpr = Expr[Name]

  type Alter[T] = (Int, List[T], Expr[T])
  type CoreAlter = Alter[Name]

  type IsRecursive = Boolean
  val recursive: IsRecursive = true
  val nonRecursive: IsRecursive = false
}
