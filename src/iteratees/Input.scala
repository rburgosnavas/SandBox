package iteratees

/**
 * states that the enumerator communicates to the iteratee
 */
sealed trait Input[+T]

// when there is data...
case class Element[T](x: T) extends Input[T]
// when there is no data
case object Empty extends Input[Nothing]
// when the end of the data is reached
case object EOF extends Input[Nothing]
