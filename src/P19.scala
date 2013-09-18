object P19 {

  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
                                                  //> list  : List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
  def rotate1[A](n: Int, xs: List[A]): List[A] = {
    def doRotate[A](num: Int, xs: List[A]): List[A] = {
      xs.drop(num) ::: xs.take(num)
    }
    if (n > 0) doRotate(n, xs) else doRotate(xs.length + n, xs)
  }                                               //> rotate1: [A](n: Int, xs: List[A])List[A]
  rotate1(3, list)                                //> res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  rotate1(-2, list)                               //> res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

}