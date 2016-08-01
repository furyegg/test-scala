package course1.week5

object ListImpl {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }
  
  def removeAt[T](n: Int, xs: List[T]) = ???
  
  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    println(init(list))
    println(removeAt(1, list))
  }
}
