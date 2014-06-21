package iteratees

/**
 * * stream consumer
 * * consumes chinks of data
 */
sealed trait Iteratee [F, T] {
  def run: T
}

// these are the steps the iteratee takes to consume data:

// cont:
case class Cont[F, T](k: Input[F] => Iteratee[F, T]) extends Iteratee [F, T] {
  override def run: T = ???
}

case class Done[F, T](result: T, remaining: Input[F]) extends Iteratee [F, T] {
  override def run: T = ???
}

case class Error[F, T](t: Throwable) extends Iteratee [F, T] {
  override def run: T = ???
}
