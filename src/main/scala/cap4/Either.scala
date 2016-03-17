package cap4

trait Either [+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight(Right(List()):Either[E, List[A]]) { (either, acc) =>
      acc match {
        case Left(e) => Left(e)
        case Right(values) => Right(values).map2(either)((vs, b) => b::vs)
      }
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val es = as.map(f)
    sequence(es)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing] {

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

  override def map[B](f: (Nothing) => B): Either[E, B] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    b match {
      case Right(vb) => Right(f(value, vb))
      case Left(e) => Left(e)
    }
  }

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))
}


object Main {

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  def main(args: Array[String]) {


    val exception = new scala.Exception("failed")
    val failed1: Either[Exception, Int] = Left(exception)
    assert(failed1.orElse(Right(10)) == Right(10))
    assert(failed1.map(_ * 2) == Left(exception))

    val success1: Either[Exception, Int] = Right(20)
    assert(success1.map(_ * 2) == Right(40))

    val v = Try(10 / 0)
    assert(v.getClass == classOf[Left[Exception]])


    //for comprehensions
    val n1 = "10"

    val toInt = for {
      v <- Try(n1.toInt)
    } yield v

    assert(toInt == Right(10))

    //for comprehensions
    val n2 = "aa"

    val toInt2 = for {
      v <- Try(n2.toInt)
    } yield v

    println(toInt2)
    assert(toInt2.getClass == classOf[Left[NumberFormatException]])

    println(Either.sequence(List(Right(10), Right(20))))
    assert(Either.sequence(List(Right(10), Right(20), Right(30))) == Right(List(10, 20, 30)))

    println(Either.sequence(List(Right(10), Left(new Exception), Right(30))))
    val left = Left(new scala.Exception())
    assert(Either.sequence(List(Right(10), left, Right(30))) == left)

    println(Either.traverse(List(10, 100, 1000))(n => Try(1000 / n)))
    assert(Either.traverse(List(10, 100, 1000))(n => Try(1000 / n)) == Right(List(100, 10, 1)))
    println(Either.traverse(List(10, 0, 1000))(n => Try(1000 / n))) // should be Left(ArithmeticException)

  }
}
