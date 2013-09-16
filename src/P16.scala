object P16 {
  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
                                                  //> list  : List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
  def drop1[A](n: Int, xs: List[A]): List[A] = {
    val indexesToDrop = for (i <- 0 to (xs.size - 1); if (i + 1) % n == 0) yield i
    xs.zipWithIndex.filter(x => !(indexesToDrop contains (x._2))) map (_._1)
  }                                               //> drop1: [A](n: Int, xs: List[A])List[A]
  drop1(3, list)                                  //> res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  def drop2[A](n: Int, xs: List[A]): List[A] = {
    xs.zipWithIndex.filter(x => (x._2 + 1) % n != 0) map (_._1)
  }                                               //> drop2: [A](n: Int, xs: List[A])List[A]
  drop2(3, list)                                  //> res1: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
}