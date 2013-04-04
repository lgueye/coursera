package recfun

import common._

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
    if (c == r || c == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRecursive(accumulator: Int, chars: List[Char]): Boolean = chars match {
      case head :: tail => {
        val newAcc = head match {
          case '(' => accumulator + 1
          case ')' => accumulator - 1
          case _ => accumulator
        }
        if (newAcc >= 0) balanceRecursive(newAcc, tail)
        else false
      }
      case Nil => accumulator == 0
    }
    balanceRecursive(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case 0          => 1
      case m if m < 0 => 0
      case _          => coins match {
        case head :: tail => countChange(money, tail) + countChange(money - head, coins)
        case Nil          => 0
      }
    }
  }

}
