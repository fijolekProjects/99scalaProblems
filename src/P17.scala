object P17 {

  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
                                                  //> list  : List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

  def split1[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    (xs.zipWithIndex.slice(0, n).map(_._1), xs.zipWithIndex.slice(n, xs.size).map(_._1))
  }                                               //> split1: [A](n: Int, xs: List[A])(List[A], List[A])

  split1(3, list)                                 //> res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 
                                                  //| 'h, 'i, 'j, 'k))
  def split2[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    xs.splitAt(n)
  }                                               //> split2: [A](n: Int, xs: List[A])(List[A], List[A])
  split2(3, list)                                 //> res1: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 
                                                  //| 'h, 'i, 'j, 'k))
  def split3[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    def mapTuple[A, B](tuple: (A, A))(f: A => B) = {
      tuple match { case (first, scnd) => (f(first), f(scnd)) }
    }
    val partitioned = xs.zipWithIndex.partition(_._2 < n)
    mapTuple(partitioned)(_.map(_._1))
  }                                               //> split3: [A](n: Int, xs: List[A])(List[A], List[A])
  split3(3, list)                                 //> res2: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 
                                                  //| 'h, 'i, 'j, 'k))
  def split4[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    (xs.take(n), xs.drop(n))
  }                                               //> split4: [A](n: Int, xs: List[A])(List[A], List[A])
  split4(3, list)                                 //> res3: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 
                                                  //| 'h, 'i, 'j, 'k))
}