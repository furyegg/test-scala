val money = 4
val coins = List(1,2)
def buildFactorsList(coinsCount: Int, maxPayCount: Int): List[Array[Int]] = {
  var factorsList = List[Array[Int]]()
  def buildFactors(factorsSize: Int, index: Int, value: Int): Array[Int] = {
    val factors = Array.fill(factorsSize)(0)
    factors(index) = value
    factors
  }
  for (c <- 0 until coinsCount) {
    for (n <- 0 to maxPayCount) {
      val factors = buildFactors(coinsCount, c, n)
      factorsList = factorsList :+ factors
    }
  }
  factorsList
}
def calAllCoins(coins: List[Int]): Int = {
  val minCoin = coins.reduceLeft((a, b) => a.min(b))
  val maxPayCount = if (money % minCoin == 0) money / minCoin else money / minCoin + 1
  val coinsCount = coins.size
  var count = 0
  val factorsList = buildFactorsList(coinsCount, maxPayCount)
  factorsList.foreach(factors => println(factors.mkString(", ")))
  factorsList.foreach(factors => {
    if (checkCoinsWithFactors(coins, factors)) count += 1
  })
  count
}
def checkCoinsWithFactors(coins: List[Int], factors: Array[Int]): Boolean = {
  var sum = 0
  var result = false;
  for (i <- 0 until factors.size) {
    sum += coins(i) * factors(i)
    if (sum == money) result = true
    if (sum > money) result = false
  }
  result
}
calAllCoins(coins)
