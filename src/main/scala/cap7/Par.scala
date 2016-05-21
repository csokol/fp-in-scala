package cap7

object ParallelComputations {

  trait Par[+A]

  def map2[A,B,C](p1: Par[A], p2: Par[B])(comb: (A,B) => C): Par[C] =  ???

}
