object P09 {
  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
                                                  //> list  : List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
  def pack1[A](xs: List[A]): List[List[A]] = xs match {
    case head :: tail => xs.takeWhile(_ == head) :: pack1(xs.dropWhile(_ == head))
    case _ => Nil
  }                                               //> pack1: [A](xs: List[A])List[List[A]]
  pack1(list)                                     //> res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c)
                                                  //| , List('a, 'a), List('d), List('e, 'e, 'e, 'e))
}