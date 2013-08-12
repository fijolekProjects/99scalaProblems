object P04 {

  def length1(xs: List[Int]): Int = {
    def lengthHelper(xs: List[Int], acc: Int): Int = {
      if (xs.isEmpty) acc
      else lengthHelper(xs.tail, acc + 1)
    }
    lengthHelper(xs, 0)
  }                                               //> length1: (xs: List[Int])Int
  length1(List(1, 1, 2, 3, 5, 8))                 //> res0: Int = 6

  def length2(xs: List[Int]): Int = xs match {
    case Nil => 0
    case _ :: tail => 1 + length2(tail)
  }                                               //> length2: (xs: List[Int])Int
  length2(List(1, 1, 2, 3, 5, 8))                 //> res1: Int = 6

  def length3(xs: List[Int]): Int = {
    def lengthHelper(xs: List[Int], acc: Int): Int = xs match {
      case Nil => acc
      case _ :: tail => lengthHelper(tail, acc + 1)
    }
    lengthHelper(xs, 0)
  }                                               //> length3: (xs: List[Int])Int
  length3(List(1, 1, 2, 3, 5, 8))                 //> res2: Int = 6

  def length4(xs: List[Int]): Int = {
    xs.foldLeft(0)((total, _) => total + 1)
  }                                               //> length4: (xs: List[Int])Int
  length4(List(1, 1, 2, 3, 5, 8))                 //> res3: Int = 6

}