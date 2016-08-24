package course2.week2

object WaterPouring {
  
  class Pouring(val capacity: Vector[Int]) {
    type State = Vector[Int]
    val initState = capacity.map(_ => 0)
    
    def pour(target: Int): List[State] = {
      Nil
    }
  }
  
  def main(args: Array[String]): Unit = {
    val pouring = new Pouring(Vector(4, 9))
    pour(pouring, 6)
  }
  
  def pour(pouring: Pouring, target: Int): Unit = {
    val steps = pouring.pour(target)
    if (steps.isEmpty) println("No way to pour " + target + " by glasses " + pouring.capacity.mkString(","))
    else steps.foreach(s => println(s.mkString("\n")))
  }
}
