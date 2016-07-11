package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], leftBracketCount: Int): Int = {
      if (chars.isEmpty)
        leftBracketCount
      else {
        val head = chars.head
        val tail = chars.tail
        if (head == '(')
          check(tail, leftBracketCount + 1)
        else if (head == ')')
          if (leftBracketCount > 0)
            check(tail, leftBracketCount - 1)
          else
            check(Nil, leftBracketCount - 1)
        else
          check(tail, leftBracketCount)
      }
    }
    check(chars, 0) == 0
  }
  
  /**
   * Exercise 3
   * http://algorithms.tutorialhorizon.com/dynamic-programming-coin-change-problem/
   * http://www.algorithmist.com/index.php/Coin_Change
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0) 0
    if (coins.isEmpty) 0
    val finalCoins = coins.filter(coin => coin > 0)

    def iterateAllCoins(coins: List[Int], total: Int): Int = {
      if (coins.isEmpty) total
      else {
        val coinsCombination = getCoinsCombination(coins)
        var n = 0
        coinsCombination.foreach(coins => n += calAllCoins(coins))

        iterateAllCoins(coins.tail, total + n)
      }
    }

    def getCoinsCombination(coins: List[Int]): List[List[Int]] = {
      val result = List[List[Int]]()
      def getSubList(result: List[List[Int]], coins: List[Int]): List[List[Int]] = {
        if (coins.isEmpty) result
        else getSubList(result :+ coins, coins.init)
      }
      getSubList(result, coins)
    }

    def buildFactorsList(coinsCount: Int, maxPayCount: Int): List[Array[Int]] = {
      var factorsList = List[Array[Int]]()

      def buildFactors(factorsSize: Int, index: Int, value: Int): Array[Int] = {
        val factors = Array.fill(factorsSize)(0)
        factors(index) = value
        factors
      }

      for (c <- 0 to coinsCount - 1) {
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
      val coinsCount: Int = coins.size
      var count = 0
      val factorsList = buildFactorsList(coinsCount, maxPayCount)
      factorsList.foreach(factors => {
        if (checkCoinsWithFactors(coins, factors)) count += 1
      })
      count
    }

    def checkCoinsWithFactors(coins: List[Int], factors: Array[Int]): Boolean = {
      var sum = 0
      var result = false;
      for (i <- 0 to factors.size - 1) {
        sum += coins(i) * factors(i)
        if (sum == money) result = true
        if (sum > money) result = false
      }
      result
    }

    iterateAllCoins(coins, 0)
  }
}
