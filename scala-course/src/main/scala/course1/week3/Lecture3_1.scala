package course1.week3

object Lecture3_1 {
  
  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }
  
  object Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    override def toString: String = "."
    override def union(other: IntSet): IntSet = other
  }
  
  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
  
    override def toString: String = "{" + left + elem + right + "}"
  
    override def union(other: IntSet): IntSet =
      if (other == Empty) this
      else ((left union right) union other) incl elem
  }
  
  def main(args: Array[String]) = {
    val s1 = Empty.incl(1)
    val s2 = s1.incl(2)
    val s3 = s2.incl(3)
    println(s3)
  
    val s4 = Empty.incl(4)
    val s5 = s4.incl(5)
    val s6 = s5.incl(6)
    println(s6)
    
    val s7 = s6.union(s3)
    println(s7)
  }
  
}
