

trait Iterator[A] {
  def next(): A
  def hasNext: Boolean
  
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    if (!hasNext) z
    else {
      var res = f(z, next())
      while (hasNext) {
        res = f(res, next())
      }
      res
    }
  }
}

trait Splitter[T] {
  def split: Seq[Splitter[T]]
  def remaining: Int
  
  val threshold = 10
  
  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children: Seq[Task[T]] = ???
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}