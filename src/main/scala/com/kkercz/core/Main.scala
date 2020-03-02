package com.kkercz.core

import com.kkercz.core.parser.Parser

case object Main {
  def main(args: Array[String]): Unit = println(Parser.parse(
    """
      | case 5 of
      |   <4> -> 4 ;
      |   <3> -> 3
      |""".stripMargin))

}
