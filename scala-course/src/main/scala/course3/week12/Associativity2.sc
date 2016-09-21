var list = List(40,30,20,10)

val sum = list.reduce(_ + _).toDouble
val length = list.map((x:Int) => 1).reduce(_ + _)
sum / length

def f = (e1: (Double, Int), e2: (Double, Int)) => (e1._1 + e2._1, e1._2 + e2._2)
val (s, len) = list.map((x: Int) => (x.toDouble,1)).reduce(f)
s / len

def average(s: Seq[Int]): Double = {
  val t = s.foldLeft((0.0, 0)) ((acc, i) => (acc._1 + i, acc._2 + 1))
  t._1 / t._2
}

def average2(s: Seq[Int]): Double =
  s.foldLeft((0.0, 1)) ((acc, i) => ((acc._1 + (i - acc._1) / acc._2), acc._2 + 1))._1

average(list)
average2(list)

def f(u: Double, v: Double): Double =
  (u + v)/(1.0 + u*v)
def err(lst:List[Double]): Double =
  lst.reduceLeft(f) - lst.reduceRight(f)
def testAssoc: Double = {
  val r = new scala.util.Random
  val lst = List.fill(400)(r.nextDouble*0.002)
  err(lst)
}
testAssoc