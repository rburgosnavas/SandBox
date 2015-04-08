package fpinscala.ch06

/**
 * Created by burgosr on 4/2/15.
 */
trait RNG {
  type Rand[+A] = RNG => (A, RNG)
  def nextInt: (Int, RNG)
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66L + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (seed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Ex. 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val i = rng.nextInt
    if (i._1 >= 0 && i._1 <= Int.MaxValue) {
      i
    } else {
      nonNegativeInt(i._2)
    }
  }

  // Ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (d, r) = nonNegativeInt(rng)
    ((d / (Int.MaxValue + 1)).toDouble, r)
  }

  // Ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(cnt: Int, curr: RNG, xs: List[Int]): (List[Int], RNG) = {
      val (x, r) = curr.nextInt
      if (cnt > 0) loop(cnt - 1, r, x::xs) else (xs, r)
    }
    loop(count, rng, Nil)
  }
}
