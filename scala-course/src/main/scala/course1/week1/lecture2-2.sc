def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}
product(x => x + 1)(1, 3)

def factorial(f: Int => Int)(a: Int, b: Int): Int = {
  def fact(a: Int, acc: Int): Int = {
    if (a > b) acc
    else fact(a + 1, f(a) * acc)
  }
  fact(a, 1)
}
factorial(x => x)(1, 4)

def combin(f: Int => Int)(g: (Int, Int) => Int)(a: Int, b: Int): Int = {
  def com(a: Int, acc: Int): Int = {
    if (a > b) acc
    else com(a + 1, f(a) * acc)
  }
  com(a, 1)
}
//combin(x => x)((y, 1) => y * 1)(1, 4)