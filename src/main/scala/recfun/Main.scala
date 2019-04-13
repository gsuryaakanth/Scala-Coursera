package recfun

object Main {
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
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balance(balancerValue: Int, t: List[Char]): Boolean = {

      if (t.isEmpty) balancerValue == 0
      else if (balancerValue < 0) false
      else if (t.head == '(') balance(balancerValue + 1, t.tail)
      else if (t.head == ')') balance(balancerValue - 1, t.tail)
      else balance(balancerValue, t.tail)
    }

    balance(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    println( "Money - " + money + "  coins " + coins)
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

}