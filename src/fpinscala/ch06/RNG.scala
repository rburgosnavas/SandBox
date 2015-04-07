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
