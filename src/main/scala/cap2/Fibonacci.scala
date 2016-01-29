package cap2

import scala.annotation.tailrec

object Fibonacci {

  // f(0)=0, f(1)=1, f(2)=1, f(3)=f(1)+f(2)=2

  def fibo1(n: Integer): Integer = {
    if (n == 1) 1
    else if (n == 0) 0
    else fibo1(n - 1) + fibo1(n - 2)
  }

  def fibo2(n: Integer): Integer = {
    @tailrec
    def go(n: Integer, currentN: Integer, f0:Integer, f1:Integer): Integer = {
      if (n == currentN) f0
      else go(n, currentN + 1, f1, f0+f1)
    }
    go(n, 0, 0, 1)
  }


  def main(array: Array[String]): Unit = {
    println(fibo2(3))
    println(fibo1(4) == fibo2(4))
    println(fibo1(5) == fibo2(5))
    println(fibo1(6) == fibo2(6))
    println(fibo1(7) == fibo2(7))
    println(fibo1(8) == fibo2(8))
    println(fibo1(9) == fibo2(9))
  }

}
