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
      |MkPair = Pack{1,2} ;
      |fst p = case p of <1> a b -> a ;
      |snd p = case p of <1> a b -> b
      |""".stripMargin
  )

  val lists: List[CoreSc] = Parser.parseCoreProgram(
    """
      |Nil = Pack{1,0} ;
      |Cons = Pack{2,2} ;
      |head = hd ;
      |hd list = case list of
      |                     <1> -> abort ;
      |                     <2> head tail -> head ;
      |tail = tl ;
      |tl list = case list of
      |                     <1> -> abort ;
      |                     <2> head tail -> tail ;
      |length = len ;
      |len list = case list of
      |                     <1> -> 0 ;
      |                     <2> h t -> 1 + len t ;
      |
      |drop n list = if (n == 0) list (case list of
      |                     <1> -> Nil ;
      |                     <2> head tail -> drop (n-1) tail) ;
      |
      |take n list = if (n == 0) Nil (case list of
      |                     <1> -> Nil ;
      |                     <2> head tail -> Cons head (take (n-1) tail)) ;
      |
      |filter f list = case list of
      |                   <1> -> Nil ;
      |                   <2> head tail -> if (f head == True)
      |                                     (Cons head (filter f tail))
      |                                     (filter f tail) ;
      |
      |map f list = case list of <1> -> Nil ; <2> head tail -> Cons (f head) (map f tail) ;
      |
      |zipWith f l1 l2 = case l1 of
      |                           <1> -> Nil ;
      |                           <2> -> Cons (f (head l1) (head l2)) (zipWith f (tail l1) (tail l2)) ;
      |zip = zipWith MkPair
      |""".stripMargin
  )

}
