package fpinscala.ch06

/**
 * Created by burgosr on 4/2/15.
 */
object RNGRunner extends App {
  val r1 = SimpleRNG(42)
  val n1 = r1.nextInt
  println(n1)
  println(n1._2.nextInt)
  println(n1._2.nextInt._2.nextInt)
  println(n1._2.nextInt._2.nextInt._2.nextInt)

  // Ex. 6.1
  println(RNG.nonNegativeInt(r1))
  println(RNG.nonNegativeInt(n1._2.nextInt._2))

  // Ex. 6.1
  println(RNG.double(n1._2))
  println(RNG.double(n1._2.nextInt._2))
  println(RNG.double(n1._2.nextInt._2.nextInt._2))

  // Ex 6.3
  println(RNG.intDouble(n1._2.nextInt._2.nextInt._2))
  println(RNG.doubleInt(n1._2.nextInt._2.nextInt._2))
  println(RNG.double3(n1._2.nextInt._2.nextInt._2))

  // Ex 6.4
  println(RNG.ints(5)(r1))
}
