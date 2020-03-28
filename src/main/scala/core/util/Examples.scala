package core.util

case object Examples {
  private val basic1 = "main = I 3"
  private val basic2 =
    """id = S K K ;
      |main = id 3""".stripMargin
  private val basic3 =
    """id = S K K ;
      |main = twice twice twice id 3""".stripMargin

  private val let1 = "main = let id1 = I I I in id1 id1 3 "
  private val let2 =     """
                           |oct g x = let h = twice g
                           |           in let k = twice h
                           |             in k (k x) ;
                           |main = oct I 4
                           |""".stripMargin
  private val let3 =     """infinite x = letrec xs = cons x xs
                           |   in xs ;
                           |main = hd (tl (tl (infinite 4)))""".stripMargin

  val sieve: String =
    """
      |main = printList (take 10 (sieve (from 2)));
      |
      |from n = cons n (from (n+1)) ;
      |
      |sieve xs = case xs of
      |       <1> -> nil ;
      |       <2> p ps -> cons p (sieve (filter (nonMultiple p) ps)) ;
      |
      |filter predicate xs
      |         = case xs of
      |               <1> -> nil ;
      |               <2> p ps -> let rest = filter predicate ps in
      |                             if (predicate p) (cons p rest) rest ;
      |
      |nonMultiple p n = ((n/p)*p) != n ;
      |
      |take n xs = if (n==0)
      |                 nil
      |                 (case xs of
      |                     <1> -> nil ;
      |                     <2> p ps -> cons p (take (n-1) ps))
      |
      |""".stripMargin

  val sieveUsingPrelude: String =
    """
      |from n = Cons n (from (n+1)) ;
      |
      |nonMultiple p n = ((n/p)*p) != n ;
      |
      |sieve xs = case xs of
      |       <1> -> nil ;
      |       <2> p ps -> Cons p (sieve (filter (nonMultiple p) ps)) ;
      |
      |main = printList (take 10 (sieve (from 2)))
      |
      |""".stripMargin

  val letrec: String =
    """
      |pair x y f = f x y ;
      |fst p = p K ;
      |snd p = p K1 ;
      |f x y = letrec
      |           a = pair x b ;
      |           b = pair y a
      |        in fst (snd (snd (snd a))) ;
      |main = f 3 4
      |""".stripMargin


  val examplePrograms = Seq(
    basic1, basic2, basic3, let1, let2, let3, sieve, letrec
  )


}
