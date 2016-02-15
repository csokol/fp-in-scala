package cap3

import scala.annotation.tailrec

/*
  [+A] -> means that List is covariant, meaning that if A extends B,
  then List[A] extends List[B].
  Practical example: Dog extends Animal -> List[Dog] extends List[Animal]
 */
sealed trait List[+A]

case object Nil extends List[Nothing] //Nothing is a subtype of every class, so List[Nothing] is a subtype of any list

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]):Int = {
    ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
  }

  def product(ints: List[Int]):Int = {
    ints match {
      case Nil => 1
      case Cons(0.0, _) => 0
      case Cons(head, tail) => head * product(tail)
    }
  }

  def apply[A](as: A*): List[A] = {
    @tailrec
    def applyT(tail: List[A], as: A*): List[A] = {
      if (as.isEmpty) {
        tail
      } else {
        applyT(Cons(as.head, tail), as.tail:_*)
      }
    }
    reverse(applyT(Nil, as: _*))
  }

  def reverse[A](list: List[A]): List[A] = {
    List.foldLeft(list, Nil:List[A])((b, a) => Cons(a, b))
  }

  /**
    * Exercise 3.2
    */
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }
  }

  /**
    * Exercise 3.3
    */
  def setHead[A](list: List[A], newHead: A): List[A] = {
    list match {
      case Nil => Nil
      case Cons(head, tail) => Cons(newHead, tail)
    }
  }

  /**
    * Exercise 3.4
    */
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n == 0) list
    else {
      list match {
        case Nil => Nil
        case Cons(head, tail) => drop(tail, n - 1)
      }
    }
  }

  /**
    * Exercise 3.5
    */
  def dropWhile[A](list: List[A], pred: (A)=>Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(head, tail) => {
        if (pred(head)) dropWhile(tail, pred)
        else Cons(head, tail)
      }
    }
  }
  /**
    * Exercise 3.6
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(last, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  /**
    * Exercise 3.9
    */
  def length[A](l: List[A]): Int = {
    List.foldRight(l, 0)((a, acc) => acc + 1)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /**
    * Exercise 3.10
    */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def foldLeftTail(as: List[A], z: B, f: (B, A) => B):B = {
      as match {
        case Nil => z
        case Cons(h, t) => foldLeftTail(t, f(z, h), f)
      }
    }

    foldLeftTail(as, z, f)
  }

  /**
    * Exercise 3.11
    */
  def sumL(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }
  def productL(ints: List[Int]): Int = {
    foldLeft(ints, 1)(_ * _)
  }
  def lengthL(ints: List[Int]): Int = {
    foldLeft(ints, 0)((acc, _) => acc + 1)
  }

  /**
    * Exercise 3.13
    */
  def foldLeftThroughFoldR[A,B](xs: List[A], acc0: B)(f: (B, A) => B): B = {
    val reversed = List.reverse(xs)
    foldRight(reversed, acc0)((a, b) => f(b,a))
  }

  def foldRightThroughFoldL[A,B](xs: List[A], acc0: B)(f: (A, B) => B): B = {
    val reversed = List.reverse(xs)
    foldLeft(reversed, acc0)((b, a) => f(a,b))
  }

  /**
    * Exercise 3.14
    */
  def appendThroughFold[A](xs: List[A], ys: List[A]): List[A] = {
    foldRight(xs, ys)(Cons(_, _))
  }

  /**
    * Exercise 3.15
    */
  def merge[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, List[A]()) { (list, merged) =>
      List.appendThroughFold(merged, list)
    }
  }

  /**
    * Exercise 3.16
    */
  def add1[A](ints: List[Int]): List[Int] = {
    foldRight(ints, Nil:List[Int])((a,b) => Cons(a + 1, b))
  }

  /**
    * Exercise 3.17
    */
  def doublesToS[A](doubles: List[Double]): List[String] = {
    foldRight(doubles, Nil:List[String])((a,b) => Cons(a.toString, b))
  }

  /**
    * Exercise 3.18
    */
  def map[A, B](as: List[A])(f: (A=>B)): List[B] = {
    foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))
  }

  /**
    * Exercise 3.19
    */
  def filter[A](as: List[A])(pred: (A=>Boolean)): List[A] = {
    foldRight(as, Nil:List[A]) { (a,b) =>
      if (pred(a)) Cons(a, b) else b
    }
  }

  /**
    * Exercise 3.20
    */
  def zipSumming(xs: List[Int], ys: List[Int]): List[Int] = {
    xs match {
      case (Nil) => Nil
      case (Cons(x, tx)) =>
        ys match {
          case Cons(y, ty) => Cons(x + y, zipSumming(tx, ty))
        }
    }
  }

  /**
    * Exercise 3.21
    */
  def zipWith[A,B,C](xs: List[A], ys: List[B])(combine: (A,B) => C): List[C] = {
    xs match {
      case (Nil) => Nil
      case (Cons(x, tx)) =>
        ys match {
          case Cons(y, ty) => Cons(combine(x, y), zipWith(tx, ty)(combine))
        }
    }
  }

  /**
    * Exercise 3.21
    */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasSubsequenceAux(sup: List[A], sub: List[A], originalSub: List[A]): Boolean = {
      sup match {
        case Nil =>
          sub match {
            case Nil => true
            case _ => false
          }
        case Cons(supHead, supTail) =>
          sub match {
            case Nil => true
            case Cons(subHead, subTail) =>
              if (subHead == supHead) hasSubsequenceAux(supTail, subTail, originalSub)
              else hasSubsequenceAux(supTail, originalSub, originalSub)
          }
      }
    }
    hasSubsequenceAux(sup, sub, sub)
  }
}


object Test extends App {

  val ints = List(1,2,3,4)
  println(ints)
  println(List.sum(ints))
  println(List.product(ints))

  println(List.product(Nil))

  /**
    * exercise 3.1 -> outputs 3
    */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  assert(3 == x)

  assert(List.setHead(ints, 666) == List(666,2, 3, 4))
  assert(List.drop(ints, 2) == List(3, 4))

  assert(List.dropWhile(ints, (i:Int) => i < 4) == List(4))

  assert(List.init(ints) == List(1,2,3))
  assert(List.length(ints) == 4)


  try {
    val hugeList = List(Range(0, 100000):_*)
    List.foldRight(hugeList, 0)(_ + _)
  } catch {
    case e:StackOverflowError => println("stackoverflow!!")
  }

  val reversed = List.foldLeft(ints, List[Int]())((b, a) => Cons(a, b))
  assert(reversed == List(4,3,2,1))

  val hugeList = List(Range(0, 100000):_*)
  val hugeSum = List.foldLeft(hugeList, 0)(_ + _)
  println(hugeSum)

  assert(List.sumL(List(1,2,3,4)) == 10)
  assert(List.productL(List(2,2,2,2)) == 16)
  assert(List.lengthL(List(2,2,2,2)) == 4)

  println("*** fold right ***")
  List.foldRightThroughFoldL(List(1,2,3,4), 0) {(a,b) =>
    println(a)
    b
  }

  println("*** fold left ***")
  List.foldLeftThroughFoldR(List(1,2,3,4), 0) {(b,a) =>
    println(a)
    b
  }

  assert(List.foldLeftThroughFoldR(List(1,2,3,4), 0)(_ + _) == 10)
  assert(List.foldRightThroughFoldL(List(1,2,3,4), 0)(_ + _) == 10)
  assert(List.appendThroughFold(List(1, 2), List(1, 2)) == List(1,2,1,2))

  assert(List.merge(List(List(1, 2), List(1, 2, 3), List(1, 2))) == List(1,2,1,2,3,1,2))

  assert(List.add1(List(1,2,3,4)) == List(2,3,4,5))
  assert(List.doublesToS(List(1,2,3,4)) == List("1.0","2.0","3.0","4.0"))

  assert(List.map(List(1,2,3,4))(_ * 2) == List(2,4,6,8))

  assert(List.filter(List(1,2,3,4))(_ % 2 == 0) == List(2,4))

  assert(List.zipSumming(List(1,2,3,4), List(1,2,3,4)) == List(2,4,6,8))

  assert(List.zipWith(List(1,2,3,4), List(1,2,3,4))(_*_) == List(1,4,9,16))

  assert(List.hasSubsequence(List(1,2,3,4), List(2,3)))
  assert(List.hasSubsequence(List(1,2,3,4), List(3,4)))
  assert(!List.hasSubsequence(List(1,2,3,4), List(1,4)))
  assert(!List.hasSubsequence(List(1,2,3,4), List(1,3)))
  assert(!List.hasSubsequence(List(1,2,3,4), List(2,1)))


}
