object P07 {
	
	def flatten(xs : List[Any]): List[Any] = xs flatMap {
		case list: List[_] => flatten(list)
		case singleElement => List(singleElement)
	}                                         //> flatten: (xs: List[Any])List[Any]
	
	flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res0: List[Any] = List(1, 1, 2, 3, 5, 8)
}