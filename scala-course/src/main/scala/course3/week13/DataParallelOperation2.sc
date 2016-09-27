import scala.collection.mutable.ArrayBuffer

var map = Map[String, ArrayBuffer[Int]]().withDefaultValue(ArrayBuffer())

val arry = map("B") += 3
map += ("B" -> arry)
map
map += ("B" -> (map("B") += 4))
map


var arr = Array.fill(5)(0)
arr.toList
arr(0) = 11
arr.toList
