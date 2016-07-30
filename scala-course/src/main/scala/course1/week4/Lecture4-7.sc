val list = List(7,3,2,9)

def print(list: List[Int]): String = list match {
  case Nil => "#"
  case y :: ys => "" + y + print(ys)
}

def isort(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => x :: Nil
  case y :: ys =>
    if (ys.isEmpty) List(y)
    else if (y < ys.head) y :: ys
    else ys.head :: insert(y, ys.tail)
}

print(list)