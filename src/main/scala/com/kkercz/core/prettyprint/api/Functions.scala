package com.kkercz.core.prettyprint.api

case object Functions {

  implicit def fromString(str: String): IStr = IStr(str)

  def iConcat(seqs: List[ISeq]): ISeq = seqs match {
    case ::(head, next) => IAppend(head, iConcat(next))
    case Nil => INil
  }

  def iInterleave(sep: ISeq, seqs: List[ISeq]): ISeq = seqs match {
    case Nil => INil
    case ::(head, Nil) => head
    case ::(head, next) => head ++ sep ++ iInterleave(sep, next)
  }
}
