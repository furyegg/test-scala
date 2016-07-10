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
  def balance2(chars: List[Char]): Boolean = {
    def check(head: Char, tail: List[Char], leftBracketCount: Int): Int = {
      if (head == '(') check(tail.head, tail.tail, leftBracketCount + 1)
      else if (head == ')') check(tail.head, tail.tail, leftBracketCount - 1)
      else if (tail.isEmpty) leftBracketCount
      else check(tail.head, tail.tail, leftBracketCount)
    }
    check(chars.head, chars.tail, 0) == 0
  }

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
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0) 0
    if (coins.isEmpty) 0
    val finalCoins = coins.filter(coin => coin > 0)

    def iterateAllCoins(coins: List[Int], total: Int): Int = {
      if (coins.isEmpty) total

      val coinsCombination = getCoinsCombination(coins)
      var n = 0
      coinsCombination.foreach(coins => n += cal(coins))

      iterateAllCoins(coins.tail, total + n)
    }

    def getCoinsCombination(coins: List[Int]): List[List[Int]] = {
      val result = List[List[Int]]()
      def getSubList(result: List[List[Int]], coins: List[Int]): List[List[Int]] = {
        if (coins.isEmpty) result
        else getSubList(result :+ coins, coins.init)
      }
      getSubList(result, coins)
    }

    def cal(coins: List[Int]): Int = {
      val factors = List[Int](coins.size)
      var sum = 0
//      while (sum > money) {
//        if (sum == money) return sum
//
//        import scala.collection.mutable.Map
//        val coinMap = Map[Int, Int]()
//        for (i <- finalCoins.size) {
//          coinMap += (finalCoins(i) -> factors(i))
//        }
//
//      }
      sum
    }

    iterateAllCoins(coins, 0)
  }
}
