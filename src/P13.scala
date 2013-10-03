object P13 {
  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
                                                  //> list  : List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
  def encodeDirect1[A](xs: List[A]): List[(Int, A)] = xs.span(_ == xs.head) match {
    case (Nil, Nil) => Nil
    case (one, rest) => (one.length, one.head) :: encodeDirect1(rest)
  }                                               //> encodeDirect1: [A](xs: List[A])List[(Int, A)]

  encodeDirect1(list)                             //> res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,
                                                  //| 'e))
}