object P20 {
  val list = List('a, 'b, 'c, 'd)                 //> list  : List[Symbol] = List('a, 'b, 'c, 'd)

  def removeAt[A](n: Int, xs: List[A]) = {
  	(xs.take(n) ::: xs.drop(n + 1), xs(n))
  }                                               //> removeAt: [A](n: Int, xs: List[A])(List[A], A)

  removeAt(3, list)                               //> res0: (List[Symbol], Symbol) = (List('a, 'b, 'c),'d)
}