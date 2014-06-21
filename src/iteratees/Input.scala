package iteratees

/**
 * states that the enumerator communicates to the iteratee
 */
sealed trait Input[+T]

// when there is data...
case class Element[T](x: T) extends Input[T]
// when there is no data
case class Empty extends Input[Nothing]
// when the end of the data is reached
case class EOF extends Input[Nothing]
