package recfun

import scala.annotation.tailrec

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
    @tailrec
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
    println("money: " + money + ", coins: " + coins)
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty && money > 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  def displayChange(money: Int, coins: List[Int], solution: List[Int], total: List[List[Int]]): List[List[Int]] = {
    if (money < 0) total
    else if (money == 0) solution :: total
    else if (coins.isEmpty && money > 0) total
    else displayChange(money - coins.head, coins, solution :+ coins.head, total) :::
        displayChange(money, coins.tail, solution, total)
  }
}
