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
}
