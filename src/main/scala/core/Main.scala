package core

import core.parser.Parser
import core.prettyprint.PrettyPrinter
import core.util.Examples

case object Main {
  def main(args: Array[String]): Unit = Examples.examplePrograms.foreach(p => { show(p); println(); println()})

  def show(program: String): Unit = println(PrettyPrinter.prettyPrint(Parser.parseCoreProgram(program)))
}
