package course2.week2

object WaterPouring {
  
  class Pouring(val capacity: Vector[Int]) {
    type State = Vector[Int]
    val initState = capacity.map(_ => 0)
    
    trait Move
    case class Empty(glass: Int) extends Move
    case class Fill(glass: Int) extends Move
    case class Pour(from: Int, to: Int) extends Move
    
    val glasses = 0 until capacity.length
    
    val moves =
      (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
  }
  
  def main(args: Array[String]): Unit = {
    val pouring = new Pouring(Vector(4, 7, 9))
    pouring.moves.foreach(println)
//    pour(pouring, 6)
  }
  
  def pour(pouring: Pouring, target: Int): Unit = {
//    val steps = pouring.pour(target)
//    if (steps.isEmpty) println("No way to pour " + target + " by glasses " + pouring.capacity.mkString(","))
//    else steps.foreach(s => println(s.mkString("\n")))
  }
}
