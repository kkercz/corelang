package com.kkercz.core.util

case object Examples {
  val examplePrograms = Seq(
    "main = I 3",
    """id = S K K ;
      |main = id 3""".stripMargin,
    """id = S K K ;
      |main = twice twice twice id 3""".stripMargin,
    """
      |oct g x = let h = twice g
      |           in let k = twice h
      |             in k (k x) ;
      |main = oct I 4
      |""".stripMargin,
    """infinite x = letrec xs = cons x xs
      |   in xs ;
      |main = hd (tl (tl (infinite 4)))""".stripMargin
  )
}
