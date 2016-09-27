def displayChange(money: Int, coins: List[Int], solution: List[Int], total: List[List[Int]]): List[List[Int]] = {
  if (money < 0) total
  else if (money == 0) solution :: total
  else if (coins.isEmpty && money > 0) total
  else displayChange(money - coins.head, coins, solution :+ coins.head, total) :::
      displayChange(money, coins.tail, solution, total)
}

val res1 = displayChange(4, List(1,2), Nil, Nil)
println(res1)