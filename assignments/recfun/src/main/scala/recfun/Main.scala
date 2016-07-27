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
    def count(money:Int, coins: List[Int], coinIndex: Int): Int = {
      println("money: " + money + ", coin index: " + coinIndex)
      if (money < 0) 0
      else if (money == 0) 1
      else if (coinIndex == coins.length && money > 0) 0
//      else count(money - coins(coinIndex), coins, coinIndex) + count(money, coins, coinIndex + 1)
      else {
        val res1 = count(money - coins(coinIndex), coins, coinIndex)
        val res2 = count(money, coins, coinIndex + 1)
        res1 + res2
      }
    }

    if (coins.isEmpty) 0
    else count(money, coins, 0)
  }
}
