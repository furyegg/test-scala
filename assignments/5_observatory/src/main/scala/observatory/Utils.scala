package observatory

import scala.math.BigDecimal.RoundingMode

object Utils {
  def avg(numbers: Iterable[Double]): Double = {
    val total = numbers.foldLeft((0, 0.0))((acc, n) => (acc._1 + 1, acc._2 + n))
    total._2 / total._1
  }
  
  def doubleToInt(d: Double): Int = {
    BigDecimal(d).setScale(0, RoundingMode.HALF_UP).toInt
  }
}
