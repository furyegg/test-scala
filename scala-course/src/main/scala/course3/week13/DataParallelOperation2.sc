val arr = Array(1,2,3,40,5)

def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

sum(arr)

def max(xs: Array[Int]): Int = {
  // xs.par.max
  xs.par.reduce((a, b) => if(a > b) a else b)
//  xs.par.reduceLeft((a, b) => if(a > b) a else b)
}

max(arr)
