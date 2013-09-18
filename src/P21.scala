object P21 {
  val list = List('a, 'b, 'c, 'd)                 //> list  : List[Symbol] = List('a, 'b, 'c, 'd)

  def insertAt[A](x: A, n: Int, xs: List[A]): List[A] = {
    val (a, b) = xs.splitAt(n)
    a ::: x :: b
  }                                               //> insertAt: [A](x: A, n: Int, xs: List[A])List[A]

  insertAt('new, 1, List('a, 'b, 'c, 'd))         //> res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

}