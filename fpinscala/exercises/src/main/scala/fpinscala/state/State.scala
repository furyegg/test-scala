package fpinscala.state

import fpinscala.state.RNG.Simple
import scala.util.Random

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text
  
  def main(args: Array[String]): Unit = {
    val rng = Simple(Random.nextLong())
    val (n, rng2) = positiveMax(10)(rng)
    println(n)
    println(positiveMax(10)(rng2))
  }

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  
  // answer 8
  def positiveInt: Rand[Int] =
    flatMap(r => r.nextInt)(a => unit(if (a < 0) (a + 1).abs else a))

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var l = List[Int]()
    var r = rng
    var i = 0
    while (i < count) {
      val (n, r2) = r.nextInt
      l = l :+ n
      r = r2
      i = i + 1
    }
    (l, r)
  }
  
  def positiveMax(n: Int): Rand[Int] =
    map(r => r.nextInt)(x => math.abs(x % (n + 1)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r => {
    val (a, r2) = ra(r)
    val (b, r3) = rb(r2)
    (f(a,b), r3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit[List[A]](List()))((l, r) => map2(r, l)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = r => {
    val (a, r1) = f(r)
    g(a)(r1)
  }
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(r => {
      val (a, r1) = run(r)
      f(a) run r1
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  
  def sequence[S,A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldLeft(unit[S, List[A]](List()))((sl, s) => s.map2(sl)(_ :: _))
  
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
  
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  
  type Rand[A] = State[RNG, A]
  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
    val Machine(_, candies, coins) = machine
    inputs.foldLeft(((candies, coins), machine))((acc, i) => (acc, i) match {
      case (((0, cns), m), _) => ((0, cns), m)
      case (((cds, cns), m@Machine(false, _, _)), Coin) => ((cds, cns), m)
      case (((cds, cns), m@Machine(true, _, _)), Turn) => ((cds, cns), m)
      case (((cds, cns), Machine(true, _, _)), Coin) => ((cds, cns + 1), Machine(false, cds, cns + 1))
      case (((cds, cns), Machine(false, _, _)), Turn) => ((cds - 1, cns), Machine(true, cds - 1, cns))
    })
  })
  
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }
  
  // WTF!!!
  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
  
  def main(args: Array[String]): Unit = {
    val m = Machine(true, 10, 0)
    val inputs = List(Coin, Coin, Turn, Turn)
    println(simulateMachine(inputs).run(m))
  }
}