object P01 {
 	
 	def last1(xs: List[Int]): Int = xs match {
 		case head :: Nil => head
 		case _ :: tail   => last1(tail)
 		case _           => throw new NoSuchElementException
 	}                                         //> last1: (xs: List[Int])Int
 	last1(List(1,2,3,4,5))                    //> res0: Int = 5
 	
 	def last2(xs: List[Int]): Int = {
 		if (xs.tail.isEmpty) xs.head
 		else last2(xs.tail)
 	}                                         //> last2: (xs: List[Int])Int
 	last2(List(1,2,3,4,5))                    //> res1: Int = 5
 	
}