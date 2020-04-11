package core.ast.prettyprint

import core.ast.Expr._
import core.ast._
import core.util.PrintableText
import core.util.PrintableText._

case object ProgramPrettyPrinter {

  def prettyPrint[T : HasName](program: Program[T]): String = ppr(program).printOut()

  def prettyPrint[T : HasName](expr: Expr[T]): String = ppr(expr).printOut()

  def ppr[T : HasName](program: Program[T]): PrintableText = interleave(" ;" ++ Newline, program map { case Supercombinator(name, vars, value) =>
    name ++ joinWithSpacePrefix(vars) ++ " = " ++ Indented(ppr(value))
  })

  def ppr[T : HasName](expr: Expr[T]): PrintableText = expr match {
    case Var(name) => implicitly[HasName[T]].getName(name)
    case Num(value) => value.toString
    case Constr(tag, arity) => s"Pack{$tag, $arity}"
    case Ap(Ap(Var(op), a), b) if Expr.operators.contains(implicitly[HasName[T]].getName(op))  => pprAtomic(a) ++ s" $op " ++ pprAtomic(b)
    case Ap(lhs, rhs) => ppr(lhs) ++ " " ++ pprAtomic(rhs)
    case Let(isRec, definitions, body) =>
      val keyword = if (isRec) "letrec" else "let"
      concat(
        keyword, " ", Indented(pprDefs(definitions)), Newline,
        "in ", Indented(ppr(body))
      )
    case Case(expr, alternatives) => concat(
      "case ", pprAtomic(expr), " of", Newline,
      "       ", Indented(interleave(" ;" ++ Newline, alternatives map { case Alter(tag, bindings, expr) =>
        "<" ++ tag.toString ++ ">" ++ joinWithSpacePrefix(bindings) ++ " -> " ++ Indented(ppr(expr))
      })))
    case Lambda(variables, body) => "\\" ++ variables.mkString(" ") ++ ". " + Indented(ppr(body))
  }

  private def pprAtomic[T : HasName](expr: Expr[T]):PrintableText = if (expr.isAtomic) ppr(expr) else "(" ++ ppr(expr) ++ ")"

  private def pprDefs[T : HasName](definitions: List[(T, Expr[T])]): PrintableText = {
    val sep = ";" ++ Newline
    val value = definitions.map(d => pprDef(d)(implicitly[HasName[T]]))
    interleave(sep, value)
  }

  private def pprDef[T : HasName](definition: (T, Expr[T])): PrintableText = definition match {
    case (withName, expr) => implicitly[HasName[T]].getName(withName) ++ " = " ++ Indented(ppr(expr))
  }

  private def joinWithSpacePrefix[T](str: List[T]): String = if (str.isEmpty) "" else " " + str.mkString(" ")
}
