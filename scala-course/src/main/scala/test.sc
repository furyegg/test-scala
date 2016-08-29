val stream = Stream(1,2,3,4)
val stream2 = Stream(7,8,9)
val stream3 = stream ++ stream2
stream3.length

def streamRange(init: Stream[Int], lo: Int, hi: Int): Stream[Int] = {
  print(lo+ " ")
  if (lo >= hi) Stream.empty
  // else Stream.cons(lo, streamRange(lo + 1, hi))
  init #::: streamRange(Stream.empty, lo + 1, hi)
}
streamRange(Stream(1,2,3), 100, 99999999).take(3).toList