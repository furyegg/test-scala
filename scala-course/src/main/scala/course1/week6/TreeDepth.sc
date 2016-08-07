sealed trait Tree[+A]
case class Branch[A](left:Tree[A],right:Tree[A] ) extends Tree[A]
case class Leaf[A](value:A) extends Tree[A]

def depth[A](tree:Tree[A]):Int = {
  def find(tree: Tree[A], current: Int, max: Int): Int = tree match {
    case Leaf(x) => if (current > max) current else max
    case Branch(l, r) =>
      val leftMax = find(l, current + 1, max)
      val rightMax = find(r, current + 1, max)
      if (leftMax > rightMax) leftMax else rightMax
  }
  find(tree, 0, 0)
}
val t1 = new Branch(new Leaf(1), new Leaf(2))
val t2 = new Branch(t1, new Leaf(3))
val t3 = new Branch(new Leaf(4), t2)
depth(t3)