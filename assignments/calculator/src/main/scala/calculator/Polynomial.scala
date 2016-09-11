package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      if (delta() < 0) Set()
      else {
        var res = Set[Double]()
        val sqrt = Math.sqrt(delta())
        res += (-b() + sqrt) / 2 * a()
        res += (-b() - sqrt) / 2 * a()
        res
      }
    })
  }
}
