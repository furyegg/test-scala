trait List[+T] {
  def isEmpty: Boolean
  
  def head: T
  
  def tail: List[T]
  
  def nth(n: Int): T
  
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object Nil extends List[Nothing] {
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

val nil = Nil
val list1 = new Cons(1, nil)
val list2 = new Cons(2, list1)
val list3 = new Cons(3, list2)
val list4 = new Cons(4, list3)
val list5 = new Cons(5, list4)

val list: List[Int] = list1
val list10 = list.prepend(Nil)

