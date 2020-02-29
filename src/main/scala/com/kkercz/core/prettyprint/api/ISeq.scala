package com.kkercz.core.prettyprint.api

sealed trait ISeq {
  def ++(to: ISeq): ISeq = IAppend(this, to)
}

case object INil extends ISeq

case class IStr(str: String) extends ISeq

case class IAppend(l: ISeq, r: ISeq) extends ISeq

case class IIndent(text: ISeq) extends ISeq

case object INewline extends ISeq