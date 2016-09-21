var list = List(1,4,10)
list.fold(100)((s, t) => s + t)
list.foldLeft(100)((s, t) => s + t)
list.reduce((s, t) => s + t)