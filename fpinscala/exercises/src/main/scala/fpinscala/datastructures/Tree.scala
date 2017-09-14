package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def count[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => count(l) + count(r)
  }
  
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  
  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
  }

}