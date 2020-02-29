package com.kkercz.core.prettyprint.api

case object AbstractPrettyPrinter {

  def display(seq: ISeq): String = flatten(0, (seq, 0) :: Nil)

  def flatten(column: Int, value: List[(ISeq, Int)]): String = value match {
    case Nil => ""
    case ::((INil, indent), next) => flatten(column, next)
    case ::((IStr(str), indent), next) => str ++ flatten(column + str.length, next)
    case ::((IAppend(left, right), indent), next) =>
      val allSeqs = (left, indent) :: (right, indent) :: next
      flatten(column, allSeqs)
    case ::((INewline, indent), next) => "\n" + " " * indent + flatten(indent, next)
    case ::((IIndent(seq), indent), next) => flatten(column, (seq, column) :: next)
  }


}
