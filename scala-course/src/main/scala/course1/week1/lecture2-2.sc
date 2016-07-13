def product2(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + product2(f)(a + 1, b)
}
product2(x => x)(1, 3)

def factorial2(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * factorial2(f)(a + 1, b)
}
factorial2(x => x)(1, 4)

def mapReduce(f: Int => Int)(cal: (Int, Int) => Int)(init: Int)(a: Int, b: Int): Int = {
  if (a > b) init
  else cal(f(a), mapReduce(f)(cal)(init)(a+1, b))
}

def product(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(x => x)((x, y) => x + y)(0)(a, b)
product(x => x)(1, 3)
def factorial(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(x => x)((x, y) => x * y)(1)(a, b)
factorial(x => x)(1, 4)