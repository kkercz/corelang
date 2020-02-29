package com.kkercz.core.prettyprint

import com.kkercz.core.ast.Types.{CoreExpr, CoreProgram, Name}
import com.kkercz.core.ast._
import com.kkercz.core.prettyprint.api.Functions.{fromString, iConcat, iInterleave}
import com.kkercz.core.prettyprint.api._

case object CorePrettyPrinter {

  def prettyPrint(program: CoreProgram): String = AbstractPrettyPrinter.display(
    iInterleave(INewline, program.map(sc => {
      val variables = joinWithSpacePrefix(sc._2, " ")
      sc._1 ++ variables ++ " = " ++ IIndent(prettyPrint(sc._3))
    }))
  )

  def prettyPrint(expr: CoreExpr): ISeq = expr match {
    case EVar(name) => name
    case ENum(value) => value.toString
    case EConstr(tag, arity) => s"Pack{$tag, $arity}"
    case EAp(EAp(EVar("+"), a), b) => prettyPrint(a) ++ " + " ++ prettyPrint(b)
    case EAp(lhs, rhs) => prettyPrint(lhs) ++ " " ++ prettyPrint(rhs)
    case ELet(isRec, definitions, body) =>
      val keyword = if (isRec) "letrec" else "let"
      iConcat(List(
        keyword, " ", IIndent(ppDefs(definitions)), INewline,
        "in ", IIndent(prettyPrint(body))
      ))
    case ECase(expr, alternatives) => "case " ++ IIndent(prettyPrint(expr)) ++ "of" ++ INewline ++
      "       " ++ IIndent(iInterleave(";" ++ INewline, alternatives.map(a =>
      "<" ++ a._1.toString ++ ">" ++ joinWithSpacePrefix(a._2, " ") ++ " -> " ++ IIndent(prettyPrint(a._3))
    )))
    case ELam(variables, body) => "\\" ++ variables.mkString(" ") ++ ". " + IIndent(prettyPrint(body))
  }

  def ppDefs(definitions: List[(Name, CoreExpr)]): ISeq = {
    val sep = ";" ++ INewline
    iInterleave(sep, definitions.map(ppDef))
  }

  def ppDef(definition: (Name, CoreExpr)): ISeq = definition._1 ++ " = " ++ IIndent(prettyPrint(definition._2))

  def joinWithSpacePrefix(str: List[String], sep: String): String = if (str.isEmpty) "" else " " + str.mkString(sep)

}
