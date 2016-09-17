package course3.week12

object MergeSort {
  def orderedMergeSort(xs: List[Int]): List[Int] = {
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(orderedMergeSort(ys), orderedMergeSort(zs))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val list = List(8,7,3,43,7,23,4,57,68,456,45,6,34,123,25)
    println(orderedMergeSort(list))
  }
}
