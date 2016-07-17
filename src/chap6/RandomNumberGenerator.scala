package chap6

trait RNG {
  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
object RNG {

  // Ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n < 0)
      (-(n + 1), nextRng)
    else
      (n, nextRng)
  }

  // Ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue + 1), nextRng)
  }

  // Ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i, d), r1)
  }

  // Ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def iter(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c == count)
        return (acc, r)
      else {
        val (i, nextR) = r.nextInt
        iter(c + 1, nextR, i :: acc)
      }
    }
    iter(0, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      {
        val (i, r) = s(rng)
        (f(i), r)
      }
  }

  // Ex 6.5
  def doubleViaRand: Rand[Double] = map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1))

  // Ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }
  }

}

object RandomNumberTest extends App {
  val rng = SimpleRNG(100)
  println("nonNegativeInt: " + RNG.nonNegativeInt(rng))
  println("double: " + RNG.double(rng))
  println("intDouble: " + RNG.intDouble(rng))
  println("ints: " + RNG.ints(5)(rng))
}