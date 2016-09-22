def scanLeft[A](inp: Array[A],
  a0: A, f: (A,A) => A,
  out: Array[A]): Unit = {
  
  out(0) = a0
  var a = a0
  var i = 0
  while (i < inp.length) {
    a = f(a, inp(i))
    i += 1
    out(i) = a
  }
}

val src = Array(1,3,8)
val dst = Array(0,0,0,0)

scanLeft[Int](src, 100, (a, b) => a+b, dst)
dst.mkString(", ")