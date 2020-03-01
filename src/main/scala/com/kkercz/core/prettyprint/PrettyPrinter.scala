package com.kkercz.core.prettyprint

import com.kkercz.core.ast.Expr._
import com.kkercz.core.ast.{CoreExpr, CoreProgram, Name}
import com.kkercz.core.util.PrintableText
import com.kkercz.core.util.PrintableText._

case object PrettyPrinter {

  def prettyPrint(program: CoreProgram): String = ppr(program).printOut()

  def ppr(program: CoreProgram): PrintableText = interleave(Newline, program map { case (name, vars, value) =>
    name ++ joinWithSpacePrefix(vars) ++ " = " ++ Indented(ppr(value))
  })

  private def ppr(expr: CoreExpr): PrintableText = expr match {
    case Var(name) => name
    case Num(value) => value.toString
    case Constr(tag, arity) => s"Pack{$tag, $arity}"
    case Ap(Ap(Var("+"), a), b) => ppr(a) ++ " + " ++ ppr(b)
    case Ap(lhs, rhs) => ppr(lhs) ++ " " ++ ppr(rhs)
    case Let(isRec, definitions, body) =>
      val keyword = if (isRec) "letrec" else "let"
      concat(
        keyword, " ", Indented(pprDefs(definitions)), Newline,
        "in ", Indented(ppr(body))
      )
    case Case(expr, alternatives) => concat(
      "case ", ppr(expr), " of", Newline,
      "       ", Indented(interleave(" ;" ++ Newline, alternatives map { case (tag, bindings, expr) =>
        "<" ++ tag.toString ++ ">" ++ joinWithSpacePrefix(bindings) ++ " -> " ++ Indented(ppr(expr))
      })))
    case Lambda(variables, body) => "\\" ++ variables.mkString(" ") ++ ". " + Indented(ppr(body))
  }

  private def pprDefs(definitions: List[(Name, CoreExpr)]): PrintableText = {
    val sep = ";" ++ Newline
    interleave(sep, definitions map pprDef)
  }

  private def pprDef(definition: (Name, CoreExpr)): PrintableText = definition match {
    case (name, expr) => name ++ " = " ++ Indented(ppr(expr))
  }

  private def joinWithSpacePrefix(str: List[Name]): String = if (str.isEmpty) "" else " " + str.mkString(" ")

}
