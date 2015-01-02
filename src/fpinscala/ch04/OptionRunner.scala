package fpinscala.ch04

/**
 * Created by burgosr on 9/22/14.
 */
object OptionRunner extends App {
  println(s"Some(10).map(_+10) = ${ Some(10) map(_+10) }")

  println(s"Some(10).flatMap(v => Some(v.toString)) = ${
    Some(10) flatMap2(v => None)
  }")

  val filteredByFlatMap = Some(10) flatMap { v =>
    if (v > 10) Some(v.toString)
    else None
  }

  println(s"filteredByFlatMap = ${filteredByFlatMap}")

  val xs = List(1.0,2.0,3.0,4.0,5.0)
  // val variance = Option.variance(xs)
  // println(s"variance = ${variance}")
  val map2res = Option.map2(Some(10), Some(5))(_+_)
  println(s"map2res = ${map2res}")

  val map3res = Option.map3(None: Option[Int], Some(2), Some(4))(_+_*_)
  println(s"map3res = ${map3res}")

  val seq = List(Some(1), Some(2), Some(3), Some(4))
  val seqRes = Option.sequence(seq)
  println(s"seqRes = ${seqRes}")
}
