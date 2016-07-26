abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new NoSuchElementException("No predecessor of Zero")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new NoSuchElementException("No Nat can less than Zero")
  
  override def toString: String = "0"
}
class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
//  def + (that: Nat): Nat = if (that.isZero) this else new Succ(this) + that.predecessor
//  def - (that: Nat): Nat =
//    if (isZero && !that.isZero) throw new NoSuchElementException("Small one can't subtract bigger one")
//    else if (that.isZero) this
//    else predecessor - that.predecessor
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
  
  override def toString: String = "" + predecessor + "#"
}


val zero = Zero
val one = new Succ(zero)
val two = new Succ(one)
val three = new Succ(two)
val four = new Succ(three)
val five = new Succ(four)

one.predecessor

one + five
five - three
