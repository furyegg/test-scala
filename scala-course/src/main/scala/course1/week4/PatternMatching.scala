package course1.week4

object PatternMatching {
  def main(args: Array[String]): Unit = {
    val e1 = Sum(Prod(Number(2), Var("x")), Var("y"))
    println(show(e1))
    
    val e2 = Prod(Sum(Number(2), Var("x")), Var("y"))
    println(show(e2))
  }
  
  trait Expr
  case class Number(n: Int) extends Expr
  case class Var(name: String) extends Expr
  case class Sum(l: Expr, r: Expr) extends Expr
  case class Prod(l: Expr, r: Expr) extends Expr
  
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }
  
  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Var(name) => name
    case Sum(l, r) => show(l) + "+" + show(r)
    case Prod(Sum(x, y), r) => "(" +  show(Sum(x, y)) + ")" + "*" + show(r)
    case Prod(l, r) => show(l) + "*" + show(r)
  }
  
}
