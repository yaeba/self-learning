package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing
    extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def checkBalance(n: Int, xs: Array[Char]): Boolean = {
      if (xs.isEmpty) n == 0
      else if (xs.head == '(') checkBalance(n + 1, xs.tail)
      else if (xs.head == ')') n > 0 && checkBalance(n - 1, xs.tail)
      else checkBalance(n, xs.tail)
    }
    checkBalance(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    // Sequential scanning and updating
    def traverse(idx: Int, until: Int, open: Int, closed: Int): (Int, Int) =
      if (idx >= until) (open, closed)
      else
        chars(idx) match {
          case '(' => traverse(idx + 1, until, open + 1, closed)
          case ')' =>
            if (open > 0) traverse(idx + 1, until, open - 1, closed)
            else traverse(idx + 1, until, open, closed + 1)
          case _ => traverse(idx + 1, until, open, closed)
        }

    // Parallel
    def reduce(from: Int, until: Int): (Int, Int) = {
      val step = until - from
      if (step <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + step / 2
        val ((left_open, left_closed), (right_open, right_closed)) =
          parallel(reduce(from, mid), reduce(mid, until))

        if (right_closed < left_open) // closed some of left open brackets
          (left_open + right_open - right_closed, left_closed)
        else (right_open, left_closed + right_closed - left_open)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
