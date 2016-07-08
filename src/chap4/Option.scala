package chap4

object OptionRunner extends App {

  trait Option[+A] {

    // Ex 4.1
    def map[B](f: A => B): Option[B] = this match {
      case None    => None
      case Some(x) => Some(f(x))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None    => None
      case Some(x) => f(x)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None    => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _    => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) if f(x) => this
      case _               => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // Ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m => mean(seqSq(xs, m)) }
  }

  def seqSq(xs: Seq[Double], mean: Double): Seq[Double] = {
    xs.map { x => (x - mean) * (x - mean) }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  object Option {

    // Ex 3.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _                  => None
    }

    // Ex 3.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      ???
    }
  }
  val seq = Seq(1.0, 2, 3)
  println("variance: " + variance(seq))
  println("variance: " + variance(Seq()))
}