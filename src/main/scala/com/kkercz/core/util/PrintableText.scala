package com.kkercz.core.util

import com.kkercz.core.util.PrintableText._

import scala.annotation.tailrec

sealed trait PrintableText {
  def ++(rhs: PrintableText): PrintableText = Append(this, rhs)

  def printOut(): String = {

    val result = new StringBuilder();

    @tailrec
    def flatten(column: Int, value: List[(PrintableText, Int)]): String =
      value match {
        case ::((Str(str), _), next) =>
          result.append(str)
          flatten(column + str.length, next)
        case ::((Indented(seq), _), next) => flatten(column, (seq, column) :: next)
        case ::((Newline, indent), next) =>
          result.append("\n" + " " * indent)
          flatten(indent, next)
        case ::((Append(left, right), indent), next) => flatten(column, (left, indent) :: (right, indent) :: next)
        case Nil => result.toString()
      }

    flatten(0, (this, 0) :: Nil)
  }
}

object PrintableText {

  case class Str(str: String) extends PrintableText

  implicit def fromString(str: String): Str = Str(str)

  case class Append(l: PrintableText, r: PrintableText) extends PrintableText

  case class Indented(text: PrintableText) extends PrintableText

  case object Newline extends PrintableText

  def concat(seqs: PrintableText*): PrintableText = seqs.fold(Str(""))(_ ++ _)

  def interleave(sep: PrintableText, seqs: List[PrintableText]): PrintableText = seqs match {
    case Nil => ""
    case ::(head, Nil) => head
    case ::(head, next) => head ++ sep ++ interleave(sep, next)
  }
}

