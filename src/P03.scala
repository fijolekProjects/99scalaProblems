object P03 {

  def nth1(num: Int, xs: List[Int]): Int = {
    if (num >= 0) xs(num)
    else throw new NoSuchElementException
  }                                               //> nth1: (num: Int, xs: List[Int])Int
  nth1(2, List(1, 1, 2, 3, 5, 8))                 //> res0: Int = 2

  def nth2(num: Int, xs: List[Int]): Int = (num, xs) match {
    case (0, head :: _) => head
    case (n, _ :: tail) => nth2(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }                                               //> nth2: (num: Int, xs: List[Int])Int
  nth2(2, List(1, 1, 2, 3, 5, 8))                 //> res1: Int = 2

}