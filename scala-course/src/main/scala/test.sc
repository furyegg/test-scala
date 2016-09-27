val list = Set(1,4,10)
val list2 = Set(11,4,10)
for {
  i <- (1 to 100).par
} yield i


list.filter(list2(_))