package course1.week4

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T
  
  def && (x: => Boolean): Boolean = ifThenElse(x, false2)
  def || (x: => Boolean): Boolean = ifThenElse(true2, x)
  def unary_! : Boolean = ifThenElse(false2, true2)

  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)
  def < (x: Boolean): Boolean = ifThenElse(false2, x)
}

object true2 extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object false2 extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = e
}