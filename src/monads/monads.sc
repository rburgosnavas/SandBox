trait OptionMonad[M] {
  def r[A](x: A): M => A
  def >>=[A, B, M](a: M)(x: A => M): M
  def >>[A, B]
}
