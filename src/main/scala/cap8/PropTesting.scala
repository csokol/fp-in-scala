package cap8

import cap6.FunctionalState.{RNG, SimpleRNG, State}

object PropTesting {

  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = {
      this.check && p.check
      ???
    }
  }

  case class Gen[A](sample: State[RNG,A]) {
    def next(rng: RNG): (A, RNG) = {
      sample(rng)
    }
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val range = stopExclusive - start
    Gen[Int]((rng) => {
      val (rndInt, nextRng) = rng.nextInt
      val offset = rndInt % range
      (start + offset, nextRng)
    })
  }

  def unit[A](a: => A): Gen[A] = {
    Gen[A](rng => (a, rng))
  }

  def boolean: Gen[Boolean] = {
    Gen[Boolean]((rng) => {
      val (rndInt, nextRng) = rng.nextInt
      (rndInt % 2 == 0, nextRng)
    })
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen[List[A]] { rng =>
      val (lastRng, l) = Range(0, n).foldLeft((rng, List[A]())) { (state, i) =>
        state match {
          case (r, list) =>
            val (a, nextRng) = g.next(r)
            (nextRng, a::list)
        }
      }
      (l, lastRng)
    }
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen[A] { rng =>
      val (b, rng1) = boolean.next(rng)
      if (b) {
        g1.next(rng1)
      } else {
        g2.next(rng1)
      }
    }
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val total = g1._2 + g2._2
    val g1Prob = g1._2/total
    val g2Prob = g2._2/total

    Gen[A] { rng =>
      val (i, rng1) = rng.nextInt
      val rndProb = Math.abs(i.toDouble) / Int.MaxValue
      if (rndProb < g1Prob) {
        g1._1.next(rng1)
      } else {
        g2._1.next(rng1)
      }
    }
  }


  def main(args: Array[String]) {
    val gen1 = choose(15, 21)
    val (g1, rng1) = gen1.next(new SimpleRNG(System.currentTimeMillis()))
    val (g2, rng2) = gen1.next(rng1)
    val (g3, rng3) = gen1.next(rng2)

    println(s"${g1}, $g2, $g3")

    val list = listOfN(10, gen1).next(rng3)._1
    println(s"$list")

    val ten = unit(10)
    val one = unit(1)

    val alwaysOne = weighted((ten, 0), (one, 1))
//    println(s"${alwaysOne.next(rng1)}, ${alwaysOne.next(rng2)}, ${alwaysOne.next(rng3)}")

    val half = weighted((ten, 3), (one, 1))

    val distribution = listOfN(1000, half).next(rng1)._1.groupBy(identity)
        .map(tup => tup._1 -> tup._2.size)
    println(s"${distribution}")
  }


}
