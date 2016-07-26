package course1.week4

abstract class Boolean2 {
  def ifThenElse[T](t: => T, e: => T): T
  
  def && (x: => Boolean2): Boolean2 = ifThenElse(x, false2)
  def || (x: => Boolean2): Boolean2 = ifThenElse(true2, x)
  def unary_! : Boolean2 = ifThenElse(false2, true2)

  def == (x: Boolean2): Boolean2 = ifThenElse(x, x.unary_!)
  def != (x: Boolean2): Boolean2 = ifThenElse(x.unary_!, x)
  def < (x: Boolean2): Boolean2 = ifThenElse(false2, x)
}

object true2 extends Boolean2 {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object false2 extends Boolean2 {
  def ifThenElse[T](t: => T, e: => T): T = e
}