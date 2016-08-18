val list = List(1,2,-3,4)
list.filter(_ > 0)
list.withFilter(_ > 0)
for {
  x <- list.withFilter(_ > 0)
} yield x

(1 -> 1.0)