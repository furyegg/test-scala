val nums: List[Int] = List(1,2,3,4,5)
nums.reduceLeft(_ + _)
nums.reduceLeft(_ * _)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x: T, n :Int) => n + 1 )

mapFun[Int, String](nums, x => "" + x + x)
lengthFun(nums)

def reverse[T](xs: List[T]): List[T] =
  (xs foldLeft List[T]())((list, x) => x :: list)

def reverse2[T](xs: List[T]): List[T] =
  (xs foldRight List[T]())((x, list) => list :+ x)

reverse(nums)
reverse2(nums)