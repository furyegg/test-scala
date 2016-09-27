

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