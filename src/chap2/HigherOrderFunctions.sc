package chap2

object HigherOrderFunctions {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  // Ex 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))                 //> curry: [A, B, C](f: (A, B) => C)A => (B => C)

  // Ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
                                                  //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C

  // Ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
                                                  //> compose: [A, B, C](f: B => C, g: A => B)A => C

}