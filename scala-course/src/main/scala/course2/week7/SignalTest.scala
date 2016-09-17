package course2.week7

object SignalTest {
  def main(args: Array[String]): Unit = {
    val s1 = Var(0)
    val s2 = Signal(s1() * 2)
  
    println(s1())
    println(s2())
    
    val v1 = s1()
    s1() = v1 + 1
    
    println(s1())
    println(s2())
    
    val s3 = foo(s1, s2)
    println(s3())
    
    s1() = 4
    println(s3())
    
    val a = Var(1.0)
    val b = Var(2.0)
    val c = Var(3.0)
    val d = Var(4.0)
    val res = computeSolutions(a,b,c,d)
    println(res())
  
    d() = 40.0
    println(res())
  }
  
  def foo(a: Signal[Int], b: Signal[Int]): Signal[String] = {
    val aa = a()
    val bb = b()
    new Signal("" + aa + bb)
    // new Signal("" + a() + b())
  }
  
  def computeSolutions(a: Signal[Double], b: Signal[Double],
    c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    var res: Set[Double] = Set()
    
    if (delta() >= 0) {
      res += (-b() + Math.sqrt(delta())) / 2 * a()
      res += (-b() - Math.sqrt(delta())) / 2 * a()
    }
    
    Signal(res)
  }
  
}