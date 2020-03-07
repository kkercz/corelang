package core.lang

import core.ast.CoreSc
import core.parser.Parser

case object Prelude {

  val builtInFunctions: List[CoreSc]  = Parser.parseCoreProgram(
    """
      |I        x       =   x              ;
      |K        x y     =   x              ;
      |K1       x y     =   y              ;
      |S        f g x   =   f x (g x)      ;
      |compose  f g x   =   f (g x)        ;
      |twice    f       =   compose f f
      |""".stripMargin)

}
