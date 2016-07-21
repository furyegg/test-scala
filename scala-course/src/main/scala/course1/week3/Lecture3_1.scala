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
  
    override def union(other: IntSet): IntSet = new NonEmpty()
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
  
    override def union(other: IntSet): IntSet = ???
  }
  
  def main(args: Array[String]) = {
    val s1 = Empty.incl(1)
    val s2 = Empty.incl(2)
    val s3 = new NonEmpty(3, s1, s2)
  
    val s4 = Empty.incl(4)
    val s5 = Empty.incl(5)
    val s6 = new NonEmpty(6, s4, s5)
    
    val s7 = s6.union(s3)
  }
  
}
