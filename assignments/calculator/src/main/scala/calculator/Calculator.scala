package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
       (name, expSig) <- namedExpressions
      valueSig = Signal(
        expSig() match {
          case Literal(v) => v
          case Ref(refName) => eval(getReferenceExpr(refName, namedExpressions), namedExpressions, List(refName))
          case Plus(a, b) => eval(a, namedExpressions, Nil) + eval(b, namedExpressions, Nil)
          case Minus(a, b) => eval(a, namedExpressions, Nil) - eval(b, namedExpressions, Nil)
          case Times(a, b) => eval(a, namedExpressions, Nil) * eval(b, namedExpressions, Nil)
          case Divide(a, b) => eval(a, namedExpressions, Nil) / eval(b, namedExpressions, Nil)
        })
    } yield (name, valueSig)
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], evaluated: List[String]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(refName) =>
        if (evaluated.contains(refName)) Double.NaN
        else eval(getReferenceExpr(refName, references), references, refName :: evaluated)
      case Plus(a, b) => eval(a, references, evaluated) + eval(b, references, evaluated)
      case Minus(a, b) => eval(a, references, evaluated) - eval(b, references, evaluated)
      case Times(a, b) => eval(a, references, evaluated) * eval(b, references, evaluated)
      case Divide(a, b) => eval(a, references, evaluated) / eval(b, references, evaluated)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
  
  def main(args: Array[String]): Unit = {
    val exp1: Expr = Literal(2)
    val exp2: Expr = Plus(Ref("e"), Literal(3))
    val exp3: Expr = Times(Ref("b"), Literal(4))
    
    val sig1 = Var(exp1)
    val sig2 = Signal(exp2)
    val sig3 = Signal(exp3)
    
    var map = Map[String, Signal[Expr]]()
    map += ("a" -> sig1)
    map += ("b" -> sig2)
    map += ("c" -> sig3)
  
    val resMap = computeValues(map)
    printMap(resMap)
  
    sig1() = Literal(4)
    printMap(resMap)
  }
  
  def printMap(map: Map[String, Signal[Double]]): Unit = {
    map.toList.foreach(e => println(s"(${e._1}=>${e._2()})"))
    println("----------------------------")
  }
}
