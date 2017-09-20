package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
    case Cons(h, _) if (n == 1) => cons(h(), empty)
    case _ => empty
  }
  
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n))({
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), nn) if nn > 1 => Some((h(), (t(), nn - 1)))
      case (Empty, _) => None
    })

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case s@Cons(_, t) =>
      if (n == 0) s
      else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }
  
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this)({
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    })
  
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if (p(h())) => t() forAll p
    case _ => false
  }
  
  def forAllViaFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
  
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, s) => cons(f(a), s))
  
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    })
  
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (f(a)) cons(a, s) else s)
  
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, s) => cons(a, s))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, s) => f(a) append  s)

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
  
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
  
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    })
  
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((a, b) => (a, b))
  
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = ???
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???
  
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3,4,5)
    println(s.take(3).toList)
    println(s.takeViaUnfold(3).toList)
    println(s.drop(2).toList)
    println(s.takeWhileViaFoldRight(_ < 3).toList)
    println(s.takeWhileViaUnfold(_ < 3).toList)
  
    println(ones.take(5).toList)
    println(ones.exists(_ % 2 != 0))
  
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
    
    println(ones.zip(alphas).takeViaUnfold(5).toList)
  }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  val alphas: Stream[String] = cons("a", alphas)
  
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(n => Some((n, n)))
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(a => Some((a, a + 1)))
  
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }
  
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))({
      case (a1, a2) => Some((a1, (a2, a1 + a2)))
    })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  
}