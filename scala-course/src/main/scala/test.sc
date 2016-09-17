var list = List(1,2,3,54,634,6,3223,2435,436,3,634,7645)
list.sum / list.length

def average(s: Seq[Int]): Double = {
  val t = s.foldLeft((0.0, 0)) ((acc, i) => (acc._1 + i, acc._2 + 1))
  t._1 / t._2
}
average(list)

def average2(s: Seq[Int]): Double =
  s.foldLeft((0.0, 1)) ((acc, i) => ((acc._1 + (i - acc._1) / acc._2), acc._2 + 1))._1
average2(list)