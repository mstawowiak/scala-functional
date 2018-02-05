package week1

object RecursionFunctions {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || r == 1 || c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], acc: Int): Boolean =
      if (chars.isEmpty && acc == 0) true
      else if (chars.isEmpty && acc > 0 || acc < 0) false
      else if (chars.head == '(') loop(chars.tail, acc + 1)
      else if (chars.head == ')') loop(chars.tail, acc - 1)
      else loop(chars.tail, acc)

    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || (money > 0 && coins.isEmpty)) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

}

