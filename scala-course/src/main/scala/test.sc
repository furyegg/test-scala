val abba = List(('a', 2), ('b', 2))

val allOccurrences: List[(Char, Int)] = for {
  e <- abba
  i <- 1 to e._2
} yield (e._1, i)

val list1 = List('a','b','c')
val list2 = List('1','2')
val list3 = List(list1, list2) ++ List(Nil)