package cap2

import scala.annotation.tailrec

object PolymorphicFunctions {

  @tailrec
  def isSorted[T](items: Array[T], greaterThan: (T,T)=>Boolean): Boolean = {
    if (items.length <= 1) true
    else {
      val t1 = items.head
      val t2 = items.tail.head
      if (greaterThan(t1, t2)) false
      else isSorted(items.tail, greaterThan)
    }
  }

  def main(args: Array[String]) {
    val greaterThan:(Int, Int) => Boolean = (t1, t2) => t1 > t2
    assert(isSorted(Array(1,2,3,4,5,5), greaterThan))
    assert(!isSorted(Array(1,2,10,4,5,5), greaterThan))
    assert(isSorted(Array(1,1,3,4,5,5), greaterThan))
  }

}
