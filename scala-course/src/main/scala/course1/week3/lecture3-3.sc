trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  
  def nth(n: Int): Nothing = throw new IndexOutOfBoundsException()
  
  override def toString(): String = "#"
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  
  def nth(n: Int): T =
    if (n < 0) throw new IndexOutOfBoundsException()
    else if (head == null) throw new IndexOutOfBoundsException()
    else if (n == 0) head
    else if (tail.isEmpty) throw new IndexOutOfBoundsException()
    else tail.nth(n - 1)
  
  override def toString(): String = "" + head + tail
}

val nil = new Nil[Int]()
val list1 = new Cons(1, nil)
val list2 = new Cons(2, list1)
val list3 = new Cons(3, list2)
val list4 = new Cons(4, list3)
val list5 = new Cons(5, list4)

list5.nth(4)
//list5.nth(-1)
list5.nth(10)
