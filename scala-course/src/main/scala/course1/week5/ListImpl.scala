package course1.week5

object ListImpl {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }
  
  def removeAt[T](n: Int, xs: List[T]): List[T] = {
    def remove(xs: List[T], index: Int): List[T] = xs match {
      case Nil => Nil
      case y :: ys => if (index == n) remove(ys, index + 1) else y :: remove(ys, index + 1)
    }
    remove(xs, 0)
  }
  
  def flatten(xs: List[Any]): List[Any] = {
    def doFlatten(xs: Any, result: List[Any]): List[Any] = xs match {
      case Nil =>
        Nil
      case y :: ys =>
        result ++ doFlatten(y, result) ++ doFlatten(ys,  result)
      case x =>
        result :+ x
    }
    doFlatten(xs, Nil)
  }
  
  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    println(init(list))
    println(removeAt(1, list))
  
    val f = flatten(List(List(1,1), 2, List(3, List(5,8))))
    println(f)
    
    // f.flatten
  }
}
