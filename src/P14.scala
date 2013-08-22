object P14 {

  val list = List('a, 'b, 'c, 'c, 'd)             //> list  : List[Symbol] = List('a, 'b, 'c, 'c, 'd)

  def duplicate1[A](xs: List[A]): List[A] = {
    xs.flatMap(x => List(x, x))
  }                                               //> duplicate1: [A](xs: List[A])List[A]
  duplicate1(list)                                //> res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def duplicate2[A](xs: List[A]): List[A] = {
    (for (x <- xs) yield List(x, x)).flatten
  }                                               //> duplicate2: [A](xs: List[A])List[A]
  duplicate2(list)                                //> res1: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def duplicate3[A](xs: List[A]): List[A] = {
    xs.flatMap(List.fill(2)(_))
  }                                               //> duplicate3: [A](xs: List[A])List[A]
  duplicate3(list)                                //> res2: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def duplicate4[A](xs: List[A]): List[A] = {
    xs.foldRight(List[A]()) { (curr, total) => List.fill(2)(curr) ::: total }
  }                                               //> duplicate4: [A](xs: List[A])List[A]
  duplicate4(list)                                //> res3: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
}