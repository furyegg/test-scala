import common._

def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A,
    f: (A,A) => A): A = {
  var sum = a0
  var i = left
  while(i < right) {
    sum = f(sum, inp(i))
    i += 1
  }
  sum
}

def mapSeg[A,B](inp: Array[A], left: Int, right: Int,
    fi : (Int,A) => B,
    out: Array[B]): Unit = {
  var i = left
  while(i < right) {
    out(i) = fi(i, inp(i))
    i += 1
  }
}

def scanLeft[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A]): Unit = {
  val fi = (i:Int, v:A) => reduceSeg1(inp, 0, i, a0, f)
  mapSeg(inp, 0, inp.length, fi, out)
  val last = inp.length - 1
  out(last + 1) = f(out(last), inp(last))
}

val src = Array(1,3,8,50)
val dst = Array.fill(5)(0)
scanLeft(src, 100, (a: Int, b: Int) => a + b, dst)
src.toList
dst.toList


sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

sealed abstract class TreeRes[A] { val res: A }
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A],
  override val res: A,
  r: TreeRes[A]) extends TreeRes[A]

def reduceRes[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
val plus = (x:Int,y:Int) => x+y
reduceRes(t1, plus)

def upsweep[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

def downsweep[A](t: TreeRes[A], a0: A, f : (A,A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, _, r) => {
    val (tL, tR) = parallel(
      downsweep[A](l, a0, f),
      downsweep[A](r, f(a0, l.res), f))
    Node(tL, tR)
  }
}

val up = upsweep(t1, plus)
val down = downsweep(up, 100, plus)