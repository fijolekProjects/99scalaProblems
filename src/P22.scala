object P22 {
  def range1(a: Int, b: Int): List[Int] = {
    (a to b).toList
  }                                               //> range1: (a: Int, b: Int)List[Int]
  range1(4, 9)                                    //> res0: List[Int] = List(4, 5, 6, 7, 8, 9)

  def range2(from: Int, to: Int): List[Int] = {
    List.range(from, to + 1)
  }                                               //> range2: (from: Int, to: Int)List[Int]
  range2(4, 9)                                    //> res1: List[Int] = List(4, 5, 6, 7, 8, 9)

  def range3(from: Int, to: Int): List[Int] = {
    if (from > to) Nil
    else from :: range3(from + 1, to)
  }                                               //> range3: (from: Int, to: Int)List[Int]
  range3(4, 9)                                    //> res2: List[Int] = List(4, 5, 6, 7, 8, 9)

  def range4(from: Int, to: Int): List[Int] = {
    def go(end: Int, acc: List[Int]): List[Int] = {
      if (from > end) acc
      else go(end - 1, end :: acc)
    }
    go(to, Nil)
  }                                               //> range4: (from: Int, to: Int)List[Int]
  range4(4, 9)                                    //> res3: List[Int] = List(4, 5, 6, 7, 8, 9)
}