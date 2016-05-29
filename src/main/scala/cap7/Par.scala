package cap7

import java.util.concurrent._

object ParallelComputations {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map[A,B](a: Par[A])(f: A=>B): Par[B] = {
    map2(a, unit()) { (a,_) =>
      f(a)
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get()
    })

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => {
      lazyUnit(f(a))
    }
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]())) { (par, finalPar)=>
      map2(finalPar, par)((l, a) => a::l)
    }
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    ???
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val result = run(es)(n)
      choices(result.get)(es)
    }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val n = map(cond) {
      case true => 0
      case false => 1
    }
    choiceN(n)(List(t, f))
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es => {
      val k = run(es)(key)
      choices(k.get)(es)
    }
  }

  def choiceNMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    val choiceByIndex = choices.zipWithIndex.map {
      case (l, index) => index -> l
    }.toMap
    choiceMap(n)(choiceByIndex)
  }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val a = run(es)(pa).get()
      choices(a)(es)
    }
  }

  def choiceNC[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(choices)
  }


  def wordCount(paragraphs: List[String]): Par[Int] = {
    val wordsPerParagraph = parMap(paragraphs) { p =>
      p.split(" ").length
    }
    map(wordsPerParagraph)(_.sum)
  }

}

object Main {
  def main(args: Array[String]) {
    import ParallelComputations._

    val par = unit(10)
    val list = sequence(List(unit(1), unit(2), unit(3)))
    val f = run(Executors.newSingleThreadExecutor())(list)
    println(f.get())
  }
}
