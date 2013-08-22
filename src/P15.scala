object P15 {
  val list = List('a, 'b, 'c, 'c, 'd)             //> list  : List[Symbol] = List('a, 'b, 'c, 'c, 'd)
  def duplicateN1[A](n: Int, xs: List[A]): List[A] = {
    xs.flatMap(List.fill(n)(_))
  }                                               //> duplicateN1: [A](n: Int, xs: List[A])List[A]
  duplicateN1(3, list)                            //> res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd
                                                  //| , 'd, 'd)
  def duplicateN2[A](n: Int, xs: List[A]): List[A] = {
    (for (x <- xs) yield List.fill(n)(x)).flatten
  }                                               //> duplicateN2: [A](n: Int, xs: List[A])List[A]
  duplicateN2(3, list)                            //> res1: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd
                                                  //| , 'd, 'd)
}