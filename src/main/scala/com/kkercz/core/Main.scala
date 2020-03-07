package com.kkercz.core

import com.kkercz.core.parser.Parser
import com.kkercz.core.prettyprint.PrettyPrinter
import com.kkercz.core.util.Examples

case object Main {
  def main(args: Array[String]): Unit = Examples.examplePrograms.foreach(p => { show(p); println(); println()})

  def show(program: String): Unit = println(PrettyPrinter.prettyPrint(Parser.parseCoreProgram(program)))
}
