val list = Set(1,4,10)
val list2 = Set(11,4,10)
for {
  i <- (1 to 100).par
} yield i


var i = 0
def test(): Boolean = {
  while (i < 10) {
    if (i > 5) return false
    println(i)
    i += 1
  }
  true
}
test