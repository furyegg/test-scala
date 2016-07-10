def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)

val n = factorial(5)

def factorial2(n: Int, m: Int): Int =
  if (m == 0) n else factorial2(n * m, n - 1)

// val n2 = factorial2(5)

def factorial3(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop (acc * n, n - 1)
  loop(1, n)
}
val n3 = factorial3(5)