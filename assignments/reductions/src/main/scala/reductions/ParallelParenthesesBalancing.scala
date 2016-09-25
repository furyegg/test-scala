package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 50,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000
    val chars = new Array[Char](length)
    val threshold = 1000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def check(chars: Array[Char], leftBracketCount: Int): Int = {
      if (chars.isEmpty)
        leftBracketCount
      else {
        val head = chars.head
        val tail = chars.tail
        if (head == '(') check(tail, leftBracketCount + 1)
        else if (head == ')')
          if (leftBracketCount > 0)
            check(tail, leftBracketCount - 1)
          else
            check(Array(), leftBracketCount - 1)
        else
          check(tail, leftBracketCount)
      }
    }
    check(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    
    @tailrec
    def traverse(idx: Int, until: Int, left: Int, right: Int): (Int, Int) = {
      if (idx == until) {
        val diff = left - right
        if (diff < 0) (0, -diff) else (diff, 0)
      } else {
        val c = chars(idx)
        val nextIdx: Int = idx + 1
        if (c == '(') traverse(nextIdx, until, left + 1, right)
        else if (c == ')')
          if (left > 0) traverse(nextIdx, until, left - 1, right)
          else traverse(nextIdx, until, 0, right + 1)
        else traverse(nextIdx, until, left, right)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold && until - from > 0) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val ((ll, lr), (rl, rr)) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        (ll - lr, rr - rl)
      }
    }

    val (l, r) = reduce(0, chars.length)
    if (l > 0 && r > 0) l - r == 0
    else false
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
