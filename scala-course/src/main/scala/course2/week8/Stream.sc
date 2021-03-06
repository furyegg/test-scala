def streamRange(lo: Int, hi: Int): Stream[Int] = {
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

val stream = streamRange(1, 10)
val s2 = 20 #:: stream