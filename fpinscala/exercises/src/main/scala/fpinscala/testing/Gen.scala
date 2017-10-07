package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state

import scala.util.Random

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//case class Prop(run: (TestCases,RNG) => Result) {
//  def check: Result = ???
//
//  // exercise 9
//  def &&(p: Prop): Prop = ???
//
//  def ||(p: Prop): Prop = ???
//}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  
//  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
//      case (a, i) => try {
//        if (f(a)) Passed else Falsified(a.toString, i)
//      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
//    }.find(_.isFalsified).getOrElse(Passed)
//  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  
  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
  
}

case class Prop(run: (MaxSize, TestCases,RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed => p.run(max, n, rng)
      case x => x
    }
  }
  
  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }
  
  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
  
//  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
//    forAll(g(_))(f)

//  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
//    (max, n, rng) =>
//      val casesPerSize = (n + (max - 1)) / max
//      val props: Stream[Prop] =
//        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
//      val prop: Prop =
//        props.map(p => Prop { (max, _, rng) =>
//          p.run(max, casesPerSize, rng)
//        }).toList.reduce(_ && _)
//      prop.run(max, n, rng)
//  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.positiveInt).map(n => start + n % (stopExclusive-start)))
  
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  
  def boolean: Gen[Boolean] =
    Gen(State(RNG.positiveMax(1)).map(n => n == 1))
  
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
  
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)
  
  // exercise 8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    ???
  
}

case class Gen[A](sample: State[RNG,A]) {
  def map[A, B](f: A => B): Gen[B] =
    Gen(sample.map(f))
  
  def flatMap[A,B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))
  
  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)
  
  // compile error, why?
  def listOfN(size: Gen[Int]): Gen[List[A]] = ???
//    size flatMap (n => this.listOfN(n))
  
  def unsized: SGen[A] = SGen(_ => this)
  
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
  
  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }
  
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }
  
  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => {
      val ga = apply(n)
      val gb = s2.g(n)
      ga.flatMap(a => gb.map(b => (a, b)))
    })
  
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => (g.listOfN(n)))
  
  def main(args: Array[String]): Unit = {
//    val smallInt = Gen.choose(-10,10)
//    val maxProp = Prop.forAll(listOf(smallInt)) { ns =>
//      val max = ns.max
//      !ns.exists(_ > max)
//    }
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {
//
//}

