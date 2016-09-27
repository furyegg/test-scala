import scala.collection.concurrent.TrieMap
import scala.collection.parallel.Splitter
import scala.collection.parallel.immutable.HashMapCombiner

def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- (0 until xs.length).par) {
    xs(0) = i
  }
}

val arr = new Array[Int](1000000)
initializeArray(arr)(10)
// arr.toList
// arr.filter(_ != 10).toList
arr(0)
