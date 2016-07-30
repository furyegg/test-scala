package course1.week4

/**
  * Created by Administrator on 2016/7/30.
  */
object SortList {
  def main(args: Array[String]): Unit = {
    println(isort(list))
  }
  
  val list = List(7,3,2,9)
  
  def isort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case y :: ys => insert(y, isort(ys))
  }
  
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => x :: Nil
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}
