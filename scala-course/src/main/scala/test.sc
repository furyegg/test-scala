val w = "asdfasdf"
val list = w.toLowerCase.toList.groupBy(e => e).toList
//list.map(((_, _)) => (_ -> _))

val list2 = List(('b' -> 2), ('a' -> 1), ('c' -> 3))
list2.map(e => (e._1 -> (e._2 * 2)))

val dictionary: List[String] = List("A", "B", "C")
