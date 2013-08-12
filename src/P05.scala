object P05 {

  def reverse1(xs: List[Int]): List[Int] = {
    def reverseHelp(xs: List[Int], acc: List[Int]): List[Int] = xs match {
      case Nil => acc
      case head :: tail => reverseHelp(tail, head :: acc)
    }
    reverseHelp(xs, Nil)
  }                                               //> reverse1: (xs: List[Int])List[Int]

  reverse1(List(1, 1, 2, 3, 5, 8))                //> res0: List[Int] = List(8, 5, 3, 2, 1, 1)

  def reverse2(xs: List[Int]): List[Int] = {
    xs.foldLeft(List(): List[Int])((processedElements, currentElement) => currentElement :: processedElements)
  }                                               //> reverse2: (xs: List[Int])List[Int]
  
  reverse2(List(1, 1, 2, 3, 5, 8))                //> res1: List[Int] = List(8, 5, 3, 2, 1, 1)

}