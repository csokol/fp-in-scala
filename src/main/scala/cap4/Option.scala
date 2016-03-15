package cap4

trait Option[+A] {
  def map[B](f: A=>B): Option[B]
  def flatMap[B](f: A=>Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => Option[B]): Option[B]
  def filter(pred: A => Boolean): Option[A]
}

object Option {
  // exercise 4.3
  def map2[A,B,C](a:Option[A], b:Option[B])(fn:(A,B) => C): Option[C] = {
    a.flatMap { actualA =>
      b.map { actualB =>
        fn(actualA, actualB)
      }
    }
  }

  // exercise 4.3
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.isEmpty) {
      Some(List())
    } else {
      val head = a.head
      head match {
        case Some(elem) => {
          sequence(a.tail).map(elem::_)
        }
        case None => None:Option[List[A]]
      }
    }
  }

  def sequenceFold[A](as: List[Option[A]]): Option[List[A]] = {
    val initialValue:Option[List[A]] = Some(List())
    as.foldRight(initialValue) { (a, acc) =>
      a match {
        case Some(elem) => acc.map(elem::_)
        case None => None
      }
    }
  }

  def traverse[A,B](as: List[A])(fn: A => Option[B]): Option[List[B]] = {
    val initialValue:Option[List[B]] = Some(List())
    as.foldRight(initialValue) { (a, acc) =>
      fn(a) match {
        case Some(elem) => acc.map(elem::_)
        case None => None
      }
    }
  }


}

object None extends Option[Nothing] {

  override def map[B](f: (Nothing) => B): Option[B] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](default: => Option[B]): Option[B] = default

  override def filter(pred: (Nothing) => Boolean): Option[Nothing] = None
}

case class Some[T](get: T) extends Option[T] {

  override def map[B](f: (T) => B): Option[B] = Some(f(get))

  override def flatMap[B](f: (T) => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: T](default: => B): B = get

  override def orElse[B >: T](default: => Option[B]): Option[B] = this

  override def filter(pred: (T) => Boolean): Option[T] = if (pred(get)) this else None
}


object Test {

  def mean(numbers: Seq[Double]): Option[Double] = {
    if (numbers.isEmpty) None
    else Some(numbers.sum / numbers.size)
  }

  def variance(numbers: Seq[Double]): Option[Double] = {
    mean(numbers).map { mean =>
      numbers.map(n => Math.pow(n - mean, 2)).sum
    }
  }

  def main(args: Array[String]) {

    val v = Some(10)
    assert(v.map(_*10) == Some(100))
    assert(v.getOrElse(42) == 10)
    assert(v.orElse(Some(42)) == Some(10))
    assert(v.filter(_ % 10 == 0) == Some(10))
    assert(v.filter(_ % 9 == 0) == None)

    val none = None
    assert(none.getOrElse(42) == 42)
    assert(none.orElse(Some(42)) == Some(42))
    assert(none.filter(_ => true) == None)

    assert(variance(List(1,1,1,1,1,1)) == Some(0.0))
    assert(variance(List()) == None)

    val a = Some(10)
    val b = Some(20)
    assert(Option.map2(a, b)(_ + _) == Some(30))
    assert(Option.map2(None:Option[Int], b)(_ + _) == None)
    assert(Option.map2(a, None:Option[Int])(_ + _) == None)

    assert(Option.sequence(List(Some(10), Some(20))) == Some(List(10, 20)))
    assert(Option.sequence(List(Some(10), Some(20), None)) == None)
    assert(Option.sequence(List(None, Some(10), Some(20))) == None)
    assert(Option.sequence(List(Some(10), None, Some(20))) == None)

    assert(Option.sequenceFold(List(Some(10), Some(20))) == Some(List(10, 20)))
    assert(Option.sequenceFold(List(Some(10), Some(20), None)) == None)
    assert(Option.sequenceFold(List(None, Some(10), Some(20))) == None)
    assert(Option.sequenceFold(List(Some(10), None, Some(20))) == None)

    assert(Option.traverse(List("10", "2", "100"))(n => Try(n.toInt)) == Some(List(10, 2, 100)))
    assert(Option.traverse(List("10", "abc", "100"))(n => Try(n.toInt)) == None)


    def Try[A](fn: => A): Option[A] = {
      try { Some(fn) }
      catch { case e:Exception => None }
    }
  }
}


