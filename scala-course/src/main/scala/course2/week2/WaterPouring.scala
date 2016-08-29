package course2.week2

object WaterPouring {
  
  class Pouring(val capacity: Vector[Int]) {
    type State = Vector[Int]
    val initState = capacity.map(_ => 0)
    
    trait Move {
      def change(state: State): State
    }
    case class Empty(glass: Int) extends Move {
      def change(state: State): State = state.updated(glass, 0)
    }
    case class Fill(glass: Int) extends Move {
      def change(state: State): State = state.updated(glass, capacity(glass))
    }
    case class Pour(from: Int, to: Int) extends Move {
      def change(state: State): State = {
        val amount = state(from).min(capacity(to) - state(to))
        state.updated(from, state(from) - amount).updated(to, state(to) + amount)
      }
    }
    
    val glasses = 0 until capacity.length
    
    val moves =
      (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
    
    class Path(history: List[Move]) {
      def endState: State = history.foldRight(initState)((move, state) => move.change(state))
      def extend(move: Move) = new Path(move :: history)
      override def toString: String = history.reverse.mkString(" ") + "--> " + endState
    }
    
    val initialPath = new Path(Nil)
    
    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves.map(path.extend)
          if (!explored.contains(next.endState))
        } yield next
        paths #:: from(more, explored ++ more.map(_.endState))
      }
    
    val pathSets = from(Set(initialPath), Set(initState))
    
    def solution(target: Int): Stream[Path] =
      for {
        pathSet <- pathSets
        path <- pathSet
        if path.endState.contains(target)
      } yield path
  }
  
  def main(args: Array[String]): Unit = {
    val pouring = new Pouring(Vector(4, 9, 12))
    // pouring.moves.foreach(println)
    // pouring.pathSets.take(5).toList.foreach(println)
    pouring.solution(6).take(5).toList.foreach(println)
  }

}
