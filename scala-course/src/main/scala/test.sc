val coins = List(1,2)
val maxPayCount = 4;
val coinsCount = coins.size

var factorsList = List[String]()
for (n <- 0 to maxPayCount) {
  for (m <- 0 to maxPayCount) {
    val factors = List[Int]()
    for (cIdx <- 0 to coinsCount - 1) {
//      for (fIdx <- 0 to coinsCount - 1) {
        cIdx :: factors
//      }
    }
    println(factors.mkString(", "))
  }
}
// println(factorsList.mkString("\n"))