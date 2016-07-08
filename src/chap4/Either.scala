package chap4

object EitherRunner extends App {

  sealed trait Either[+E, +A] {
    
    // Ex 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e)  => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_)  => b
      case Right(_) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        aa <- this
        bb <- b
      } yield (f(aa, bb))
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
}