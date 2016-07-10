def getAllCoinsCombination(coins: List[Int]): List[List[Int]] = {
  val result = List[List[Int]]()
  def getSubList(result: List[List[Int]], coins: List[Int]): List[List[Int]] = {
    if (coins.isEmpty) result
    else getSubList(result :+ coins, coins.init)
  }
  getSubList(result, coins)
}

val list = List(1,2,3)
getAllCoinsCombination(list)