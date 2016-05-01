package cap6

object FunctionalState {

  type Rand[+A] = RNG => (A, RNG)

  def unit[S,A](a: A): S => (A, S) = rng => (a, rng)

  def map[S,A,B](s: S => (A, S))(f: A => B): S =>(B,S)  = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def flatMap[S,A,B](s: S => (A, S))(f: A => (S => (B, S))): S => (B, S) = rng => {
    val (a, rng2) = s(rng)
    val (b, rng3) = f(a)(rng2)
    (b, rng3)
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { a =>
      unit(f(a))
    }
  }

  def nonNegativeLessThen(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) + mod >= 0) (mod, rng2) else nonNegativeLessThen(n)(rng2)
  }

  def nonNegativeLessThenFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) + mod >= 0) unit(mod) else nonNegativeLessThen(n)
    }
  }

  def map2[S,A,B,C](ra: S => (A, S), rb: S => (B, S))(f: (A, B) => C): S => (C, S) = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a,b), rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((a, b) => (a,b))
  }

  def randIntDouble(rng: RNG): ((Int, Double), RNG) = {
    both(int, double)(rng)
  }
  def randDoubleInt(rng: RNG): ((Double, Int), RNG) = {
    both(double, int)(rng)
  }

  def sequence[S, A](fs: List[S => (A, S)]): S => (List[A], S) = rng => {
    if (fs.isEmpty) {
      (List(), rng)
    } else {
      val (h, rng2) = fs.head(rng)
      val (t, rng3) = sequence(fs.tail)(rng2)
      (h::t, rng3)
    }
  }

  def intsSeq(count: Int, rng: RNG): (List[Int], RNG) = {
    val rands = List.fill(count)((rng:RNG) => rng.nextInt)
    sequence(rands)(rng)
  }

  def doubleMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue)(rng)
  }

  def double2Map(rng: RNG): ((Double, Double), RNG) = {
    map2(nonNegativeInt, nonNegativeInt) { (i1, i2) =>
      (i1.toDouble/Int.MaxValue, i2.toDouble/Int.MaxValue)
    }(rng)
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r1, nextRng) = rng.nextInt
    val r = if (r1 == Int.MinValue) r1 + 1 else r1

    val nonNegative = if (r < 0) -r else r

    (nonNegative, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (r1, nextRng) = nonNegativeInt(rng)

    val d = r1.toDouble / Int.MaxValue

    (d, nextRng)
  }

  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    } else {
      val (i, rng2) = rng.nextInt
      val (l, rf) = ints(count - 1)(rng2)
      (i::l, rf)
    }
  }

  type State[S, +A] = S => (A, S)
  object State {
    def map[S,A,B](s: State[S, A])(f: A => B): State[S, B]  = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def flatMap[S,A,B](s: State[S, A])(f: A => State[S, B]): State[S, B] = rng => {
      val (a, rng2) = s(rng)
      val (b, rng3) = f(a)(rng2)
      (b, rng3)
    }

    def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] = rng => {
      if (fs.isEmpty) {
        (List(), rng)
      } else {
        val (h, rng2) = fs.head(rng)
        val (t, rng3) = sequence(fs.tail)(rng2)
        (h::t, rng3)
      }
    }
  }


  def main(args: Array[String]) {
    val rng1 = SimpleRNG(System.currentTimeMillis)

    val (r1, rng2) = rng1.nextInt
    println(r1)


    val (nonNegative, rng3) = nonNegativeInt(rng1)
    println(nonNegative)

    val (d, rng4) = double(rng1)
    println(d)


    val (d2, _) =  doubleMap(rng1)
    println(d2)

    val (l, _) =  ints(10)(rng1)
    println(l)
    val (l2, _) =  intsSeq(10, rng1)
    println(l2)


    val (lt1, rng5) = nonNegativeLessThenFlatMap(20)(rng4)
    println(lt1)
    val (lt2, rng6) = nonNegativeLessThenFlatMap(20)(rng5)
    println(lt2)

    def negativeInt = mapWithFlatMap(nonNegativeInt)(i => -i)
    val (n1, rng7) = negativeInt(rng6)
    println(n1)
    val (n2, rng8) = negativeInt(rng7)
    println(n2)

  }

}
