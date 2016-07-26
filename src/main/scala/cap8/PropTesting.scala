package cap8

import cap6.FunctionalState.{RNG, SimpleRNG, State}

object PropTesting {

  case class SGen[+A](forSize: Int => Gen[A]) {
    def next(rng: RNG): (A, RNG) = {
      val gen = forSize(0)
      gen.sample(rng)
    }
  }

  case class Gen[A](sample: State[RNG,A]) {
    def next(rng: RNG): (A, RNG) = {
      sample(rng)
    }

    def unsized: SGen[A] = {
      SGen(x => this)
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n, g))
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

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case class Prop(run: (TestCases,RNG) => Result) {
      def &&(p: Prop): Prop = {
        Prop {
          (n, rng) => {
            val run1 = this.run(n, rng)
            if (run1.isFalsified) {
              run1
            } else {
              p.run(n, rng)
            }
          }
        }
      }

      def ||(p: Prop): Prop = {
        Prop {
          (n, rng) => {
            val run1 = this.run(n, rng)
            val run2 = p.run(n, rng)
            if (!run1.isFalsified || !run2.isFalsified) {
              if (!run1.isFalsified) run1 else run2
            } else {
              run1
            }
          }
        }
      }
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    import cap5.Stream

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
    }


    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
      Stream.unfold(rng)(rng => Some(g.sample(rng)))
    }
    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

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
    println(s"${alwaysOne.next(rng1)}, ${alwaysOne.next(rng2)}, ${alwaysOne.next(rng3)}")

    val half = weighted((ten, 3), (one, 1))

    val distribution = listOfN(1000, half).next(rng1)._1.groupBy(identity)
        .map(tup => tup._1 -> tup._2.size)
    println(s"${distribution}")

    val p1 = Prop.forAll(choose(1, 100))(_ <= 100)
    val p2 = Prop.forAll(choose(1, 100))(_ <= 20)
    val and = p1 && p2
    val or = p1 || p2
    println(and.run(100, rng1))
    println(or.run(1000, rng1))
  }


}
