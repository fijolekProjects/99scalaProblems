object P18 {
  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'start, 'j, 'end)
                                                  //> list  : List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'start, 'j, 'end)
                                                  //| 
  def slice1[A](start: Int, end: Int, xs: List[A]): List[A] = {
    xs.zipWithIndex.filter(x => x._2 >= start && x._2 < end).map(_._1)
  }                                               //> slice1: [A](start: Int, end: Int, xs: List[A])List[A]
  slice1(3, 7, list)                              //> res0: List[Symbol] = List('d, 'e, 'f, 'g)

  def slice2[A](start: Int, end: Int, xs: List[A]): List[A] = {
    xs.slice(start, end)
  }                                               //> slice2: [A](start: Int, end: Int, xs: List[A])List[A]
  slice2(3, 7, list)                              //> res1: List[Symbol] = List('d, 'e, 'f, 'g)

  def slice3[A](start: Int, end: Int, xs: List[A]): List[A] = {
    xs.diff(xs.take(end - (start + 1)) ::: xs.diff(xs.take(end)))
  }                                               //> slice3: [A](start: Int, end: Int, xs: List[A])List[A]
  slice3(3, 7, list)                              //> res2: List[Symbol] = List('d, 'e, 'f, 'g)

  list.drop(3).dropRight(7 - 3)                   //> res3: List[Symbol] = List('d, 'e, 'f, 'g)

  def slice4[A](start: Int, end: Int, xs: List[A]): List[A] = {
    xs.drop(start).dropRight(end - start)
  }                                               //> slice4: [A](start: Int, end: Int, xs: List[A])List[A]
  slice4(3, 7, list)                              //> res4: List[Symbol] = List('d, 'e, 'f, 'g)
  
  def slice5[A](start: Int, end: Int, xs: List[A]): List[A] = {
    xs.drop(start).take(end - start)
  }                                               //> slice5: [A](start: Int, end: Int, xs: List[A])List[A]
  slice5(3, 7, list)                              //> res5: List[Symbol] = List('d, 'e, 'f, 'g)
}