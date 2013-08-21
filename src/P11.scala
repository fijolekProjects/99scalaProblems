object P11 {
  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
                                                  //> list  : List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
  def pack1[A](xs: List[A]): List[List[A]] = xs match {
    case head :: tail => xs.takeWhile(_ == head) :: pack1(xs.dropWhile(_ == head))
    case _ => Nil
  }                                               //> pack1: [A](xs: List[A])List[List[A]]

  def encodeModified2[A](xs: List[A]): List[Any] = {
    val packedXs = pack1(xs)
    packedXs.foldRight(List[Any]()) {
      (curr, total) =>
        if (curr.length == 1) (curr(0)) :: total
        else (curr.length, curr(0)) :: total
    }
  }                                               //> encodeModified2: [A](xs: List[A])List[Any]
  encodeModified2(list)                           //> res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  def encodeModified3[A](xs: List[A]): List[Any] = {
    val packedXs = pack1(xs)
    packedXs.map {
      x =>
        if (x.length == 1) x.head
        else (x.length, x.head)
    }
  }                                               //> encodeModified3: [A](xs: List[A])List[Any]

  encodeModified3(list)                           //> res1: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

}