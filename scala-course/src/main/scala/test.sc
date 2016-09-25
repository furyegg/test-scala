var list = List(1,4,10)
list.fold(100)((s, t) => s + t)
list.foldLeft(100)((s, t) => s + t)
list.reduce((s, t) => s + t)
list.scanLeft(100)((s, t) => s + t)

var l = List.fill(10)(2)

val chars = "(if (zero? x) max (/ 1 x))".toCharArray
chars.length
chars
chars.zipWithIndex.foreach(println)