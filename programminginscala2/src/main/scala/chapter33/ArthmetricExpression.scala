object ArthmetricExpression {
  
  import scala.util.parsing.combinator._
  
  sealed trait Tree {
    def expr: Expr
  }
  case class Leaf(expr: Expr) extends Tree
  case class Fork(l: Tree, r: Tree, expr: Expr) extends Tree
  
  sealed trait Expr
  case class Number(num: Double) extends Expr
  // case class UnOp(f: (Expr) => Expr) extends Expr
  case class BinOp[T <: Expr](f: (T, T) => T) extends Expr {
    def calc = f
  }
  
  object Add extends BinOp((op1: Number, op2: Number) => op1 + op2)
  object Sub extends BinOp((op1: Number, op2: Number) => op1 - op2)
  object Mul extends BinOp((op1: Number, op2: Number) => op1 * op2)
  object Div extends BinOp((op1: Number, op2: Number) => op1 / op2)
  
  implicit final class addNumber(private val self: Number) extends AnyVal {
    def +(other: Number): Number = Number(self.num + other.num)
  }
  implicit final class subNumber(private val self: Number) extends AnyVal {
    def -(other: Number): Number = Number(self.num - other.num)
  }
  implicit final class multiNumber(private val self: Number) extends AnyVal {
    def *(other: Number): Number = Number(self.num * other.num)
  }
  implicit final class divNumber(private val self: Number) extends AnyVal {
    def /(other: Number): Number = Number(self.num / other.num)
  }
  
  object Arith extends JavaTokenParsers {
    def expr: Parser[Tree] = term ~ rep(
        "+" ~> term ^^ { case term => (Add, term) } |
        "-" ~> term ^^ { case term => (Sub, term) }
    ) ^^ {
      case term ~ Nil => term
      case term ~ rest => buildNode(term, rest)
    }
    def term: Parser[Tree] = factor ~ rep(
        "*" ~> factor ^^ { case factor => (Mul, factor) } |
        "/" ~> factor ^^ { case factor => (Div, factor) }
    ) ^^ {
      case factor ~ Nil => factor
      case factor ~ rest => buildNode(factor, rest)
    }
    def factor: Parser[Tree] = floatingPointNumber ^^ (n => Leaf(Number(n.toDouble))) |
        "(" ~> expr <~ ")"
    
    private def buildNode(l: Tree, nodes: List[(Expr, Tree)]): Tree = nodes.foldLeft(l)((l, node) => Fork(l, node._2, node._1))
  }
  
  def evaluate(tree: Tree): Expr = tree match {
    case Leaf(expr) => expr
    case Fork(l, r, op: BinOp[Expr]) => op.calc(evaluate(l), evaluate(r))
  }
  
  def main(args: Array[String]) = {
    val exp = "(2 * (3 + 7) + 5) / 5 - 1"
    val tree = Arith.parseAll(Arith.expr, exp).get
    println(tree)
    
    val res = evaluate(tree)
    val value = res match {
      case Number(n) => n
      case _ => "Error"
    }
    println(value)
  }
  
  // why this not work???
  //object Arith {
  //  def main(args: Array[String]) = {
  //    val exp = "2 * (3 + 7)"
  //    println(parseAll(expr, exp))
  //  }
  //}
  
//  class Arith2 extends JavaTokenParsers {
//    def expr: Parser[Any] = term ~ rep(
//      "+" ~ term ^^ { case "+" ~ term => " + " + term } |
//        "-" ~ term ^^ { case "-" ~ term => " - " + term }
//    ) ^^ {
//      case term ~ Nil => term
//      case term ~ rest => "" + term + rest
//    }
//    def term: Parser[Any] = factor ~ rep(
//      "*" ~ factor ^^ { case "*" ~ factor => " * " + factor } |
//        "/" ~ factor ^^ { case "/" ~ factor => " / " + factor }
//    ) ^^ {
//      case factor ~ Nil => factor
//      case factor ~ rest => "" + factor + rest
//    }
//    def factor: Parser[Any] = floatingPointNumber |
//      "(" ~> expr <~ ")" ^^ { case expr => "( " + expr + " )" }
//  }
  
}