object P08 {

  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
                                                  //> list  : List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
  def compress1[A](xs: List[A]): List[A] = {
    def compresHelp[A](xs: List[A], acc: List[A]): List[A] = xs match {
      case fst :: scnd :: tail if fst != scnd => compresHelp(scnd :: tail, acc ::: List(fst))
      case fst :: scnd :: tail => compresHelp(scnd :: tail, acc)
      case fst :: Nil => compresHelp(Nil, acc ::: List(fst))
      case Nil => acc
    }
    compresHelp(xs, Nil)
  }                                               //> compress1: [A](xs: List[A])List[A]
  compress1(list)                                 //> res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  def compress2[A](xs: List[A]): List[A] = xs match {
    case head :: tail => head :: compress2(tail.dropWhile(_ == head))
    case Nil => Nil
  }                                               //> compress2: [A](xs: List[A])List[A]
  compress2(list)                                 //> res1: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  def compress3[A](xs: List[A]): List[A] = {
    def compressHelp[A](xs: List[A], acc: List[A]): List[A] = xs match {
      case head :: tail => compressHelp(tail.dropWhile(_ == head), acc ::: List(head))
      case Nil => acc
    }
    compressHelp(xs, Nil)
  }                                               //> compress3: [A](xs: List[A])List[A]
  compress3(list)                                 //> res2: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  def compress4[A](xs: List[A]): List[A] = {
    xs.foldRight(List[A]()) {
      (curr, total) =>
        if (total.isEmpty || total.head != curr) curr :: total
        else total
    }
  }                                               //> compress4: [A](xs: List[A])List[A]
  compress4(list)                                 //> res3: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

}