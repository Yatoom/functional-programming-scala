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
    if (c == 0 || r == 0 || r == c) {
      1
    } else {
      // pascal(1, 2) = pascal(0, 1) + pascal(1, 1)
      // pascal(1, 3) = pascal(0, 2) + pascal(1, 2)
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceInner(depth: Int, chars: List[Char]): Boolean = {
      if (depth < 0) {
        false
      } else if (chars.isEmpty) {
        depth == 0
      } else if (chars.head == '(') {
        balanceInner(depth + 1, chars.tail)
      } else if (chars.head == ')') {
        balanceInner(depth - 1, chars.tail)
      } else {
        balanceInner(depth, chars.tail)
      }
    }
    balanceInner(0, chars)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
