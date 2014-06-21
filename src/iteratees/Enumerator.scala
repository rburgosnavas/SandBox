package iteratees

/**
 * * stream producer
 * * enumerator -> iteratee
 * * folds iteratee over stream
 */
sealed trait Enumerator[F] {
  def apply[T](iter: Iteratee[F, T]): Iteratee[F, T]
  def run[T](iter: Iteratee[F, T]): T
}

