package chap5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  // Ex 5.1
  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // Ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty                => sys.error("take on empty stream not allowed")
    case Cons(h, t) if n > 1  => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case _ if n == 0         => this
    case Empty               => sys.error("drop on empty stream not allowed")
    case Cons(h, t) if n > 0 => t().drop(n - 1)
  }

  // Ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _                    => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  // Ex 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // Ex 5.5
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) Stream.cons(a, b)
      else Stream.empty)

  // Ex 5.6
  def headOption(): Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  // Ex 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) =>
      if (f(h)) Stream.cons(h, t)
      else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h) append t)

  // Ex 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(streamA =>
      streamA match {
        case Empty      => None
        case Cons(h, t) => Some((f(h()), t()))
      })

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (stream, count) =>
        stream match {
          case Cons(h, t) if count == 1 => Some((h(), (Empty, 0)))
          case Cons(h, t) if count > 1  => Some((h(), (t(), count - 1)))
          case _                        => None
        }
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _                      => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
      case _                            => None
    }

  // Ex 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s)
      .takeWhile(
        {
          case (_, Some(_)) => true
          case (_, _)       => false
        })
      .forAll { case (a, b) => a == b }

  // Ex 5.15
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Cons(_, t) => Some(this, t())
      case Empty      => None
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Ex 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val constStream: Stream[A] = Stream.cons(a, constStream)
    constStream
  }

  // Ex 5.9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  // Ex 5.10
  def fibStream(): Stream[Int] = {
    def fibInternal(first: Int, second: Int): Stream[Int] = {
      cons(first, fibInternal(second, first + second))
    }
    fibInternal(0, 1)
  }

  // Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // Ex 5.12
  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1)) { case (first, second) => Some((first, (second, first + second))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def constantViaUnfold(n: Int): Stream[Int] =
    unfold(n)(_ => Some((n, n)))
}

object StreamRunner extends App {

  val st = Stream(1, 2, 3, 4)
  val s2 = Stream(11, 12, 13, 14, 15)

  println("toList: " + st.toList)
  println("take: " + st.take(3).toList)
  println("drop: " + st.drop(2).toList)
  println("takeWhile: " + st.takeWhile { _ < 3 }.toList)
  println("forAll: " + st.forAll { _ > 1 })
  println("takeWhileUsingFoldRight: " + st.takeWhileUsingFoldRight { _ < 3 }.toList)
  println("headOption: " + st.headOption())
  println("headOption with None: " + Stream().headOption())
  println("map: " + st.map { _ * 2 }.toList)
  println("filter: " + st.filter { _ > 2 }.toList)
  println("append: " + st.append(Stream(10, 9)).toList)
  println("flatMap: " + st.flatMap { x => Stream(x, x * x) }.toList)
  println("ones: " + Stream.constant(1).take(4).toList)
  println("natural numbers: " + Stream.from(10).take(5).toList)
  println("fibonacci: " + Stream.fibStream().take(10).toList)
  println("multiples of 3: " + Stream.unfold(0)(x => Option((x, x + 3))).take(5).toList)
  println("fibs via unfold: " + Stream.fibsViaUnfold().take(5).toList)
  println("from via unfold: " + Stream.fromViaUnfold(10).take(5).toList)
  println("constant via unfold: " + Stream.constantViaUnfold(6).take(5).toList)
  println("map via unfold: " + st.mapViaUnfold { _ * 2 }.toList)
  println("take via unfold: " + st.takeViaUnfold(3).toList)
  println("takeWhile via unfold: " + st.takeWhileViaUnfold { _ < 3 }.toList)
  println("zipWith: " + st.zipWith(s2)(_ + _).take(3).toList)
  println("zipAll: " + st.zipAll(s2).take(5).toList)
  println("startsWith: " + st.startsWith(Stream(1, 2)))
}