package core.lang

import core.ast.{CoreSc, Expr, Name}
import core.parser.Parser

case object Prelude {

  val falseLiteral: Expr.Constr[Name] = Expr.Constr(1, 0)
  val trueLiteral: Expr.Constr[Name] = Expr.Constr(2, 0)
  val pairLiteral: Expr.Constr[Name] = Expr.Constr(3, 2)
  val nilLiteral: Expr.Constr[Name] = Expr.Constr(4, 0)
  val consLiteral: Expr.Constr[Name] = Expr.Constr(5, 2)

  val basicFunctions: List[CoreSc]  = Parser.parseCoreProgram(
    """
      |I        x       =   x              ;
      |K        x y     =   x              ;
      |K1       x y     =   y              ;
      |S        f g x   =   f x (g x)      ;
      |compose  f g x   =   f (g x)        ;
      |twice    f       =   compose f f
      |""".stripMargin)

  val structuredData: List[CoreSc] = Parser.parseCoreProgram(
    """
      |False = Pack{1,0} ;
      |True = Pack{2,0} ;
      |if c t e = case c of
      |           <1> -> e ;
      |           <2> -> t ;
      |
      |MkPair = Pack{3,2} ;
      |fst p = case p of <3> a b -> a ;
      |snd p = case p of <3> a b -> b ;
      |
      |Nil = Pack{4,0} ;
      |Cons = Pack{5,2} ;
      |head = hd ;
      |hd list = case list of
      |                     <4> -> abort ;
      |                     <5> head tail -> head ;
      |tail = tl ;
      |tl list = case list of
      |                     <4> -> abort ;
      |                     <5> head tail -> tail ;
      |length = len ;
      |len list = case list of
      |                     <4> -> 0 ;
      |                     <5> h t -> 1 + len t
      |""".stripMargin
  )

}
