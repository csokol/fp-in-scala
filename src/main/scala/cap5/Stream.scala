package cap5

sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(hd, _) => Some(hd())
    }
  }

  def toList: List[A] = {
    this match {
      case Empty => List[A]()
      case Cons(hd, tail) => hd() :: tail().toList
    }
  }

  // exercise 5.1
  def take(n: Int): Stream[A] = {
    if (n == 0) {
      Empty
    } else {
      this match {
        case Empty => Empty
        case Cons(hd, tail) => {
          Cons(hd, () => tail().take(n - 1))
        }
      }
    }
  }

  // exercise 5.2
  def drop(n: Int): Stream[A] = {
    if (n == 0) {
      this
    } else {
      this match {
        case Empty => Empty
        case Cons(hd, tail) => {
          tail().drop(n - 1)
        }
      }
    }
  }

  // exercise 5.3
  //how can I make this lazy?
  def takeWhile(pred: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(hd, tail) => {
        lazy val predResult = pred(hd())
        if (predResult) {
          tail().takeWhile(pred)
        } else {
          Cons(hd, tail)
        }
      }
    }
  }

  // exercise 5.4
  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(hd, tail) => p(hd()) && tail().forAll(p)
    }
  }

  // exercise 5.5
  def foldRight[B](z: B)(f: (A, =>B) => B): B = {
    this match {
      case Empty => z
      case Cons(hd, tail) => f(hd(), tail().foldRight(z)(f))
    }
  }

  //this is wrong :(
  def takeWhile2(pred: (A) => Boolean): Stream[A] = {
    this.foldRight(Empty:Stream[A]) { (elem, stream) =>
      if (pred(elem)) {
        Cons(() => elem, () => stream)
      } else {
        stream
      }
    }
  }

  // exercise 5.7
  def map[B](fn: A => B): Stream[B] = {
    this.foldRight(Empty:Stream[B]) { (elem, stream) =>
      Cons(() => fn(elem), () => stream)
    }
  }
  def filter(pred: A => Boolean): Stream[A] = {
    this.foldRight(Empty:Stream[A]) { (elem, stream) =>
      if (pred(elem)) {
        Cons(() => elem, () => stream)
      } else {
        stream
      }
    }
  }

  // exercise 5.13
  def mapUnfold[B](fn: A=>B): Stream[B] = {
    Stream.unfold(this) {
      case Empty => None
      case Cons(hd, tail) => Some(fn(hd()), tail())
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(hd, tail), x) => Some(hd(), (tail(), x-1))
    }
  }
  def takeWhileUnfold(pred: A=>Boolean): Stream[A] = {
    Stream.unfold(this) {
      case (Empty) => None
      case Cons(hd, tail) =>
        if (pred(hd())) Some(hd(), tail()) else None
    }
  }
  def zipWith[B,C](other: Stream[B])(combine: (A,B) => C): Stream[C] = {
    Stream.unfold((this, other)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(hd1, tail1), Cons(hd2, tail2)) => {
        val c = combine(hd1(), hd2())
        Some(c, (tail1(), tail2()))
      }
    }
  }

  def zipAll[B](other: Stream[B]): Stream[(A,B)] = {
    Stream.unfold((this, other)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(hd1, tail1), Cons(hd2, tail2)) => {
        Some((hd1(), hd2()), (tail1(), tail2()))
      }
    }
  }

  // exercise 5.14
  def startsWith[A](other: Stream[A]): Boolean = {
    val bools = Stream.unfold((this, other, true)) {
      case (_, _, false) => None
      case (_, Empty, true) => None
      case (Empty, _, _) => Some((false, (Empty, Empty, false)))
      case (Cons(hd1, tail1), Cons(hd2, tail2), _) => {
        val eq = hd1() == hd2()
        Some((eq, (tail1(), tail2(), eq)))
      }
    }
    bools.forAll(_ == true)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def append[A](s1: Stream[A], s2: Stream[A]): Stream[A] = {
    s1.foldRight(s2) { (a, appended) =>
      Cons(() => a, () => appended)
    }
  }


  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(): Stream[Int] = {
    def nextFib(n:Int, n1:Int): Stream[Int] = {
      val next = n + n1
      cons(next, nextFib(n1, next))
    }
    cons(0, cons(1, nextFib(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val v1 = f(z)
    val possibleStream = v1 map { tuple =>
      cons(tuple._1, unfold(tuple._2)(f))
    }
    possibleStream.getOrElse(Empty)
  }

  def fibsUnfold(): Stream[Int] = {
    val nextFibs = unfold((0, 1)) { s =>
      val next = s._1 + s._2
      Some(next, (s._2, next))
    }
    cons(0, cons(1, nextFibs))
  }

  def fromUnfold(n: Int): Stream[Int] = {
    unfold(n) { s =>
      Some(s+1, s+1)
    }
  }

  def constantUnfold(k: Int): Stream[Int] = {
    unfold(k) { s =>
      Some(s, s)
    }
  }

}


object Test {
  def main(args: Array[String]) {
    import Stream.cons

    val s1 = cons(build(10), Empty)

    println(s"Stream built: ${s1}")

    println(s1.headOption)
    println(s1.headOption)


    println(s"\n==== TESTING toList ====")
    val s2 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s2.toList)
    println(s2.toList)

    println(s"\n==== TESTING take(n) ====")

    val s3 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s"should not evaluate vals")
    println(s3.take(2))
    println(s"now it should evaluate")
    println(s3.take(2).toList)
    println(s3.take(2).toList)

    println(s"\n==== TESTING drop(n) ====")

    val s4 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s"should not evaluate vals")
    println(s4.drop(2))
    println(s"now it should evaluate only third value")
    println(s4.drop(2).toList)
    println(s4.drop(2).toList)

    println(s"\n==== TESTING takeWhile(n) ====")

    val s5 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s"should not evaluate vals")
    println(s5.takeWhile(_ <= 2))
    println(s"now it should evaluate only third value")
    println(s5.takeWhile(_ <= 2).toList)
    println(s5.takeWhile(_ <= 2).toList)

    println(s"\n==== TESTING forall ====")

    val s6 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s"it should evaluate only first and second")
    println(s6.forAll(_ < 2))
    println(s6.forAll(_ < 2))

    println(s"\n==== TESTING takeWhile2 ====")

    val s7 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s7.takeWhile2(_ % 2 == 1).toList)

    println(s"\n==== TESTING map ====")

    val s8 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s"should not evaluate vals")
    println(s8.map(_ * 2))
    println(s"now it should evaluate values once")
    println(s8.map(_ * 2).toList)
    println(s8.map(_ * 2).toList)

    println(s"\n==== TESTING filter ====")

    val s9 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    println(s"should not evaluate vals")
    println(s9.filter(_ % 2 == 0).toList)

    println(s"\n==== TESTING append ====")

    val s10 = cons(build(1), cons(build(2), cons(build(3), Empty)))
    val s11 = cons(build(4), cons(build(5), cons(build(6), Empty)))
    println(s"should not evaluate vals")
    val appended = Stream.append(s10, s11)
    println(s"now evaluate it!")
    println(appended.toList)

    println(s"\n==== TESTING infinite streams ====")

    val s12 = Stream.constant(10)
    println(s12.take(10).toList)
    val naturalNumbers = Stream.from(10)
    println(naturalNumbers.take(10).toList)
    println(s"fibs: ${Stream.fibs().take(10).toList}")
    println(s"fibs unfold: ${Stream.fibsUnfold().take(10).toList}")
    println(s"from unfold: ${Stream.from(20).take(10).toList}")
    println(s"constant unfold: ${Stream.constant(42).take(10).toList}")
    println(s"map unfold: ${Stream.from(20).map(_ * 2).take(10).toList}")
    println(s"take unfold: ${Stream.from(20).takeUnfold(10).toList}")
    println(s"takeWhile unfold: ${Stream.from(20).takeWhileUnfold(_ < 40)}")
    println(s"takeWhile unfold: ${Stream.from(20).takeWhileUnfold(_ < 40).toList}")
    val seq = Stream.from(20)
    val threes = Stream.constant(3)
    println(s"zipWith unfold: ${seq.zipWith(threes)(_ * _).take(10).toList}")
    println(s"zipAllunfold: ${Stream.from(1).zipAll(Stream.from(101)).take(10).toList}")

    println(s"\n==== TESTING startsWith ====")
    println(s"should be true: ${Stream.from(1).startsWith(Stream(1,2,3,4))}")
    println(s"should be false: ${Stream.from(1).startsWith(Stream(2,3,4))}")



  }

  def build(i: Int): Int = {
    println(s"evaluating ${i}")
    i
  }
}