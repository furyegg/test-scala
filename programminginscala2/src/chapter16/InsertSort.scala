object InsertSort {
	def isort(xs: List[Int]): List[Int] = xs match {
		case List() => List()
		case x :: xs1 => insert(x, isort(xs1))
	}

	def insert(x: Int, xs: List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y :: ys => if (x <= y) x :: xs
			            else y :: insert(x, ys)
	}

	def main(args: Array[String]) {
		val list = List(8,123,5,3,4645,6,213,23)
		val sorted = isort(list)
		sorted.foreach(println)
	}
}