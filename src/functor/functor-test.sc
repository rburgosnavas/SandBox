trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorF {
  def fmap[A, B, C](func: A => B)(f: B => C): A => C = {
    val functor = new Functor[({type L[B] = A => B})#L] {
      override def fmap[B, C](fa: A => B)(f: B => C): A => C = (a: A) => f(fa(a))
    }
    functor.fmap(func)(f)
  }
}

val fii: Int => Int = i => i * 2
val fis: Int => String = i => s"i = $i"
FunctorF.fmap(fii)(fis)(10)

sealed trait Maybe[+A]
case class Just[A](a: A) extends Maybe[A]
case object None extends Maybe[Nothing]

object MaybeF extends Functor[Maybe] {
  override def fmap[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = fa match {
    case Just(a) => Just(f(a))
    case None => None
  }
}

val op: Int => Int = _ * 5
MaybeF.fmap(Just(1000))(op)
MaybeF.fmap(None)(op)
