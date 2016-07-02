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
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)
    }

  }

  val list = List(1, 3, 4, 7, 5)

  println(evaluateX())
  println(List.tail(list))
  println(List.setHead(list, 5))
  println(List.drop(list, 2))
  println(List.dropWhile(list, (i: Int) => i < 5))

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