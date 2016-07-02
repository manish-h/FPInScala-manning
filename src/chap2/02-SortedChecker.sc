package chap2

import scala.annotation.tailrec

object SortedChecker {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  // Ex 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(i: Int): Boolean = {
      if (i == as.length - 1)
        true
      else if (ordered(as(i), as(i + 1)))
        loop(i + 1)
      else
        false
    }
    loop(0)
  }                                               //> isSorted: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean

  def intComparater(x: Int, y: Int) = x < y       //> intComparater: (x: Int, y: Int)Boolean
  isSorted(Array(1, 2, 3, 4), intComparater)      //> res0: Boolean = true
  isSorted(Array(1, 4, 3, 2), intComparater)      //> res1: Boolean = false
}