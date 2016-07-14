import math.abs

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println("guess = " + guess)
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}
def sqrt(x: Double): Double = fixedPoint(y => (y + x / y) / 2)(1)
sqrt(2)

def averageDamp(x: Double): Double => Double = ???

def sqrt2(x: Double): Double = fixedPoint(averageDamp(y => x / y))(1)
sqrt2(2)