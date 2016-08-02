package course1.week5

object ListImpl {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }
  
  def removeAt[T](n: Int, xs: List[T]) = xs.take(n) ::: xs.drop(n + 1)

  def flatten(xs: List[Any]): List[Any] = {
    def doFlatten(xs: List[Any], result: List[Any]): List[Any] = xs match {
      case Nil => Nil
      case y :: ys => {
        val headList = y match {
          case list: List[_] => doFlatten(list, result)
          case x: Any => List(x)
        }
        result ++ headList ++ doFlatten(ys, result)
      }
    }
    doFlatten(xs, Nil)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    println(init(list))
    println(removeAt(1, list))

    val list2 = List(List(1,3),2, List(5,List(7,8)))
    println(flatten(list2))
  }
}