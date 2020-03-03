package com.kkercz.core

import com.kkercz.core.parser.Parser
import com.kkercz.core.prettyprint.PrettyPrinter

case object Main {
//  def main(args: Array[String]): Unit = Examples.examplePrograms.foreach(p => { show(p); println(); println()})
  def main(args: Array[String]): Unit = show(
    """
      |infinite x = letrec xs = cons x xs
      |   in xs ;
      |main = hd (tl (tl (infinite 4)))""".stripMargin
  )


  def show(program: String) = println(PrettyPrinter.prettyPrint(Parser.parse(program)))
}
