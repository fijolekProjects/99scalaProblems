object P12 {
  val list = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
                                                  //> list  : List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4
                                                  //| ,'e))
  def decode1[A](xs: List[(Int, A)]): List[A] = {
    def decodeOne[A](one: (Int, A)): List[A] = {
      def go[A](one: (Int, A), acc: List[A]): List[A] = {
        if (one._1 > 0) go((one._1 - 1, one._2), one._2 :: acc)
        else acc
      }
      go(one, List[A]())
    }
    xs.flatMap(decodeOne(_))
  }                                               //> decode1: [A](xs: List[(Int, A)])List[A]
  decode1(list)                                   //> res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e
                                                  //| , 'e)
                
  def decode2[A](xs: List[(Int, A)]): List[A] = {
    def decodeOne[A](one: (Int, A)): List[A] = {
      if (one._1 > 0) one._2 :: decodeOne((one._1 - 1, one._2))
      else Nil
    }
    xs.flatMap(decodeOne(_))
  }                                               //> decode2: [A](xs: List[(Int, A)])List[A]
  decode2(list)                                   //> res1: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e
                                                  //| , 'e)
            
  def decode3[A](xs: List[(Int, A)]): List[A] = {
    def decodeOne[A](one: (Int, A)): List[A] = {
      if (one._1 > 0) one._2 :: decodeOne((one._1 - 1, one._2))
      else Nil
    }
    xs.foldRight(List[A]()) {
      (curr, total) => decodeOne(curr) ::: total
    }
  }                                               //> decode3: [A](xs: List[(Int, A)])List[A]
  decode3(list)                                   //> res2: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e
                                                  //| , 'e)
             
  def decode4[A](xs: List[(Int, A)]): List[A] = {
    for (one <- xs; howMany <- 1 to one._1) yield one._2
  }                                               //> decode4: [A](xs: List[(Int, A)])List[A]
  decode4(list)                                   //> res3: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
             
  def decode5[A](xs: List[(Int, A)]): List[A] = {
    xs.flatMap {
      one => for (howMany <- 1 to one._1) yield one._2
    }
  }                                               //> decode5: [A](xs: List[(Int, A)])List[A]
  decode5(list)                                   //> res4: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
             
  def decode6[A](xs: List[(Int, A)]): List[A] = {
    xs.flatMap {
      one => (1 to one._1).map(howMany => one._2)
    }
  }                                               //> decode6: [A](xs: List[(Int, A)])List[A]
  decode6(list)                                   //> res5: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
             
  def decode7[A](xs: List[(Int, A)]): List[A] = {
    xs.flatMap(one => List.make(one._1, one._2))
  }                                               //> decode7: [A](xs: List[(Int, A)])List[A]
  decode7(list)                                   //> res6: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, '
                                                  //| e, 'e)
}