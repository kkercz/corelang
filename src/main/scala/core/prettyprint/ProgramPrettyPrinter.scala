package core.prettyprint

import core.ast.Expr._
import core.ast._
import core.util.PrintableText
import core.util.PrintableText._

case object ProgramPrettyPrinter {

  def prettyPrint(program: CoreProgram): String = ppr(program).printOut()

  def prettyPrint(expr: CoreExpr): String = ppr(expr).printOut()

  def ppr(program: CoreProgram): PrintableText = interleave(" ;" ++ Newline, program map { case Supercombinator(name, vars, value) =>
    name ++ joinWithSpacePrefix(vars) ++ " = " ++ Indented(ppr(value))
  })

  private def ppr(expr: CoreExpr): PrintableText = expr match {
    case Var(name) => name
    case Num(value) => value.toString
    case Constr(tag, arity) => s"Pack{$tag, $arity}"
    case Ap(Ap(Var(op), a), b) if Expr.operators.contains(op)  => pprAtomic(a) ++ s" $op " ++ pprAtomic(b)
    case Ap(lhs, rhs) => ppr(lhs) ++ " " ++ pprAtomic(rhs)
    case Let(isRec, definitions, body) =>
      val keyword = if (isRec) "letrec" else "let"
      concat(
        keyword, Newline,
        "       ", Indented(pprDefs(definitions)), Newline,
        "in ", Indented(ppr(body))
      )
    case Case(expr, alternatives) => concat(
      "case ", pprAtomic(expr), " of", Newline,
      "       ", Indented(interleave(" ;" ++ Newline, alternatives map { case Alter(tag, bindings, expr) =>
        "<" ++ tag.toString ++ ">" ++ joinWithSpacePrefix(bindings) ++ " -> " ++ Indented(ppr(expr))
      })))
    case Lambda(variables, body) => "\\" ++ variables.mkString(" ") ++ ". " + Indented(ppr(body))
  }

  private def pprAtomic(expr: CoreExpr):PrintableText = if (expr.isAtomic) ppr(expr) else "(" ++ ppr(expr) ++ ")"

  private def pprDefs(definitions: List[(Name, CoreExpr)]): PrintableText = {
    val sep = ";" ++ Newline
    interleave(sep, definitions map pprDef)
  }

  private def pprDef(definition: (Name, CoreExpr)): PrintableText = definition match {
    case (name, expr) => name ++ " = " ++ Indented(ppr(expr))
  }

  private def joinWithSpacePrefix(str: List[Name]): String = if (str.isEmpty) "" else " " + str.mkString(" ")
}
