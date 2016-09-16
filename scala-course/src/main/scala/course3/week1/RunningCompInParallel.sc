def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s;
  var sum: Int = 0
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}

def power(x: Int, p: Double): Int = math.exp(p * math.log(math.abs(x))).toInt

//def pNormTwoPart(a: Array[Int], p: Double): Int = {
//  val m = a.length / 2
//  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
//  power(sum1 + sum2, 1 / p)
//}