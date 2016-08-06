def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, second) = xs.span(_ == x)
    first :: pack(second)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(list => (list.head, list.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))
// List(("a", 3), ("b", 1), ("c", 2), ("a", 1))