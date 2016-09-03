package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  lazy val genHeap: Gen[H] = for {
    x <- Gen.choose(0, 1000000)
//    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)
  
  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin_default") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("findMin2") = forAll(empty) { (h: H) =>
    val x = 4
    val y = 6
    var heap = insert(x, empty)
    heap = insert(y, heap)
    findMin(heap) == x
  }

  property("deleteMin1") = forAll(empty) { (h: H) =>
    val x = 4
    var heap = insert(x, empty)
    heap = deleteMin(heap)
    isEmpty(heap)
  }

  property("hasRightOrder") = forAll { (h: H) =>
    def checkOrder(lastMin: Option[A], heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val min = findMin(heap)
        val rest = deleteMin(heap)
        if (!lastMin.isDefined || lastMin.get < min) checkOrder(Some(min), rest)
        else false
      }
    }
//    println(h)
    checkOrder(None, h)
  }
  
  property("meld") = forAll { (h1: H, h2: H) =>
    val heap = meld(h1, h2)
    val min = findMin(heap)
    min == findMin(h1) || min == findMin(h2)
  }

}
