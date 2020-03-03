package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r) throw new IndexOutOfBoundsException()
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkBalance(n: Int, xs: List[Char]): Boolean = {
      if (xs.isEmpty) n == 0
      else if (xs.head == '(') checkBalance(n + 1, xs.tail)
      else if (xs.head == ')') n > 0 && checkBalance(n - 1, xs.tail)
      else checkBalance(n, xs.tail)
    }
    checkBalance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countWays(remain: Int, coins: List[Int]): Int = {
      if (remain == 0) 1
      else if (remain < 0 || coins.isEmpty) 0
      else if (coins.head <= 0) throw new Exception("Coin value <= 0")
      else countWays(remain - coins.head, coins) + countWays(remain, coins.tail)
    }

    if (money == 0 || coins.isEmpty) 0
    else countWays(money, coins)
  }
}
