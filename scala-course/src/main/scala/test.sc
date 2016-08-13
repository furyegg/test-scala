val abba = List(('a', 1), ('b', 2))
abba.map(e => (e._1 -> e._2)).toMap.updated('b', 3)