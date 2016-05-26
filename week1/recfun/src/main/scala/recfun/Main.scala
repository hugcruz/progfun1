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
      if(r<0 || c<0 || c>r) 0
      else if(c==0 || c==r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balanceCount(chars: List[Char], cnt: Int): Int = {
      if(cnt == -1) -1
      else if(chars.isEmpty) cnt
      else if(chars.head == '(') balanceCount(chars.tail, cnt + 1)
      else if(chars.head == ')') balanceCount(chars.tail, cnt - 1)
      else balanceCount(chars.tail, cnt)
    }

    def balance(chars: List[Char]): Boolean = {
      balanceCount(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0) 0
      else if(coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
