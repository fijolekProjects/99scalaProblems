object P10 {
  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
                                                  //> list  : List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
  def pack1[A](xs: List[A]): List[List[A]] = xs match {
    case head :: tail => xs.takeWhile(_ == head) :: pack1(xs.dropWhile(_ == head))
    case _ => Nil
  }                                               //> pack1: [A](xs: List[A])List[List[A]]

  def encode1[A](xs: List[A]): List[(Int, A)] = {
    val packedXs = pack1(xs)
    for {
      list <- packedXs
    } yield (list.length, list(0))
  }                                               //> encode1: [A](xs: List[A])List[(Int, A)]

  encode1(list)                                   //> res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,
                                                  //| 'e))
  def encode2[A](xs: List[A]): List[(Int, A)] = {
    val packedXs = pack1(xs)
    packedXs.foldRight(List[(Int, A)]()) {
      (curr, total) => (curr.length, curr(0)) :: total
    }
  }                                               //> encode2: [A](xs: List[A])List[(Int, A)]
  encode2(list)                                   //> res1: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,
                                                  //| 'e))
  def encode3[A](xs: List[A]): List[(Int, A)] = {
    val packedXs = pack1(xs)
    packedXs.map(x => (x.length, x.head))
  }                                               //> encode3: [A](xs: List[A])List[(Int, A)]

  encode3(list)                                   //> res2: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,
                                                  //| 'e))
}