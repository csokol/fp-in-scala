package cap2

import scala.annotation.tailrec

object Factorial {

  // f(0)=0, f(1)=1, f(2)=1, f(3)=f(1)+f(2)=2

  def fact(n: Integer): Integer = {
    @tailrec
    def go(n: Integer, acc: Integer):Integer = {
      if (n <= 1) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }


  def main(array: Array[String]): Unit = {
    assert(fact(0) == 1)
    assert(fact(1) == 1)
    assert(fact(2) == 2)
    assert(fact(3) == 6)
    assert(fact(4) == 24)
  }

}
