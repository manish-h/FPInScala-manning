package chap3

import scala.annotation.tailrec

object ListRunner extends App {
  sealed trait List[+A]
  case object Nil extends List[Nothing] {
    override def toString = "Nil"
  }
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def toString = head + "::" + tail
  }

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Ex 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil         => throw new IllegalArgumentException("tail of Nil list not allowed")
      case Cons(_, xs) => xs
    }

    // Ex 3.3
    def setHead[A](l: List[A], newHead: A): List[A] = l match {
      case Nil         => throw new IllegalArgumentException("setHead on Nil not allowed")
      case Cons(_, xs) => Cons(newHead, xs)
    }

    // Ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = {

      @tailrec
      def loop(list: List[A], curr: Int): List[A] = {
        if (curr == n) list
        else loop(List.tail(list), curr + 1)
      }

      loop(l, 0)
    }

    // Ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil         => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

    // Ex 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil                   => Nil
      case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
      case Cons(x, xs)           => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // Ex 3.9
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

    // Ex 3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

      @tailrec
      def loop(as: List[A], acc: B): B = as match {
        case Nil         => acc
        case Cons(x, xs) => loop(xs, f(acc, x))
      }

      loop(as, z)
    }

    // Ex 3.11
    def sumUsingFoldLeft(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

    def lengthUsingFoldLeft[A](list: List[A]): Int = foldLeft(list, 0)((a: Int, b: A) => a + 1)

    // Ex 3.12
    def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((xs, x) => Cons(x, xs))

    // Ex 3.14
    def append[A](left: List[A], right: List[A]): List[A] = foldRight(left, right)(Cons(_, _))

    // Ex 3.15
    def concat[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])(append)

    // Ex 3.16
    def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

    // Ex 3.17
    def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))

    // Ex 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, xs) => Cons(f(x), xs))

    // Ex 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

    // Ex 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

    // Ex 3.21
    def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    // Ex 3.22
    def zipAdd(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b)(_ + _)

    // Ex 3.23
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (_, Nil)                     => Nil
      case (Nil, _)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    // Ex 3.24
    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil)                     => true
      case (Nil, _)                     => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
    }

  }

  val list = List(1, 3, 4, 7, 5)

  println("evaluateX " + evaluateX())
  println("tail " + List.tail(list))
  println("setHead " + List.setHead(list, 5))
  println("drop " + List.drop(list, 2))
  println("dropWhile " + List.dropWhile(list, (i: Int) => i < 5))
  println("init " + List.init(list))
  println("length " + List.length(list))
  println("sum using foldLeft " + List.sumUsingFoldLeft(list))
  println("length using foldLeft " + List.lengthUsingFoldLeft(list))
  println("reverse using foldLeft " + List.reverse(list))
  println("append " + List.append(list, list))
  println("concat " + List.concat(List(list, list, list)))
  println("add  1 " + List.addOne(list))
  println("double to string " + List.doubleToString(List(1.3, 3.3)))
  println("map " + List.map(list)(_ * 2))
  println("filter " + List.filter(list)(_ > 4))
  println("flatMap " + List.flatMap(list)(a => Cons(a, Cons(a * a, Nil))))
  println("zipAdd " + List.zipAdd(List(1, 3, 5), List(2, 4, 5, 6)))
  println("has subsquence " + List.hasSubsequence(list, List(3, 4, 7)))

  // Ex 3.1
  def evaluateX(): Int = {
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + List.sum(t)
      case _                                     => 101
    }
  }

}