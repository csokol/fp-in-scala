package cap2

object Currying {

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a:A) => (b:B) => f(a, b)
  }

  def uncurry[A,B,C](f: A =>B => C): (A,B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f:B=>C, g:A=>B): A => C = {
    (a:A) => f(g(a))
  }

  def main(args: Array[String]) {
    val mult = (x:Int, y:Int) => x*y

    val curryied = curry(mult)

    val mult2 = curryied(2)
    val mult3 = curryied(3)
    val mult4 = curryied(4)

    println(mult2(2) == 4)
    println(mult3(2) == 6)
    println(mult4(2) == 8)

    val uncurryed = uncurry(curryied)

    println("======")
    println(uncurryed(2,2) == 4)
    println(uncurryed(2,3) == 6)
    println(uncurryed(2,4) == 8)

    val dobra = (x:Int) => x*2
    val dividePorDois = (x:Int) => x/2

    val identidade = compose(dividePorDois, dobra)

    println("======")
    println(identidade(10) == 10)
    println(identidade(11) == 11)
    println(identidade(12) == 12)

    val quadriplica = compose(dobra, dobra)

    println("======")
    println(quadriplica(10) == 40)
    println(quadriplica(20) == 80)
    println(quadriplica(30) == 120)


  }

}
