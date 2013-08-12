object P02 {

  def penultimate1(xs: List[Int]): Int = xs match {
    case _ :: almostLast :: last :: Nil => almostLast
    case head :: tail => penultimate1(tail)
    case _ => throw new NoSuchElementException
  } //> penultimate1: (xs: List[Int])Int
  penultimate1(List(1, 1, 2, 3, 5, 8)) //> res0: Int = 5

  def penultimate2(xs: List[Int]): Int = {
    if (xs.tail.tail.isEmpty) xs.head
    else penultimate2(xs.tail)
  } //> penultimate2: (xs: List[Int])Int
  penultimate2(List(1, 1, 2, 3, 5, 8)) //> res1: Int = 5

}