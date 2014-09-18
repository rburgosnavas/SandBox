package fpinscala.ch03

/******************************************************************************
 * List trait
 *****************************************************************************/
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/******************************************************************************
 * List
 *****************************************************************************/
object List {

  // Sum (recursive)
  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(x, xx) => x + sum(xx)
  }

  // Sum (tail-recursive)
  def sumTR(xs: List[Int]): Int = {
    def loop(curr: Int, next: List[Int]): Int = next match {
      case Nil => curr
      case Cons(x, xx) => loop((x + curr), xx)
    }
    loop(0, xs)
  }

  // Product (recursive)
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Product (tail-recursive)
  def productTR(ds: List[Double]): Double = {
    def loop(curr: Double, next: List[Double]): Double = next match {
      case Nil => curr
      case Cons(x, xs) => loop((curr * x), xs)
    }
    loop(1.0, ds)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // def reverse[A](as: List[A]): List[A] = {
  //   def loop(acc: List[A], curr: List[A]): List[A] = curr match {
  //     case Nil => acc
  //     case Cons(h, t) => loop(Cons(h, acc), t)
  //   }
  //   loop(Nil, as)
  // }

  // Ex. 2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(a, aa) => aa
  }

  // Ex. 3
  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(l, ls) => Cons(a, ls)
  }

  // Ex. 4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (n > 0) drop(xs, n - 1) else Cons(x, xs)
  }

  def tailDrop[A](as: List[A]): List[A] = drop(as, 1)

  // Ex. 5
  // (not currying)
  // def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //   case Nil => l
  //   case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)
  // }

  // -- revised (currying)
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else Cons(x, xs)
  }

  // append
  // TODO fix me!
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    def loop(acc: List[A], me: List[A], that: List[A]): List[A] = me match {
      case Nil => acc
      case Cons(h, t) => loop(Cons(h, acc), t, that)
    }
    loop(Nil, a1, a2)
  }

  // Ex. 6
  def init[A](l: List[A]): List[A] = {
    def loop[A](curr: List[A], acc: List[A]): List[A] = curr match {
      case Nil => acc
      case Cons(x, Nil) => acc
      case Cons(x, xs) => loop(xs, Cons(x, acc))
    }
    loop(l, Nil)
  }

  def foldRight[A, B](as: List[A])(zero: B)(f: (A, B) => B): B = as match {
    case Nil => zero
    case Cons(h, t) => f(h, foldRight(t)(zero)(f))
    // this case is like a foldLeft recursion: for a zero = Nil: List[A]
    // and Cons(_, _), the result is a reversed List[A]...
    // case Cons(h, t) => foldRight(t)(f(h, zero))(f)
  }

  // Ex. 9
  def length[A](as: List[A]): Int =
    foldRight(as)(0)((_, y) => y + 1)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    def loop(curr: List[A], acc: B): B = curr match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h))
    }
    loop(as, z)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as)(Nil: List[A])((b, a) => Cons(a, b))
  }

  // Ex. 13
  def foldRight2[A, B](as: List[A])(zero: B)(f: (A, B) => B): B =
    foldLeft(as)(zero)((b, a) => f(a, b))

  // Ex. 14
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1))(a2)((b, a) => Cons(a, b))

  // Ex. 15
  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as)(Nil: List[A])(appendFold)

  def incIntList(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, incIntList(t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def mapTR[A, B](as: List[A])(f: A => B): List[B] = {
    def loop(curr: List[A], acc: List[B]): List[B] = curr match {
      case Nil => reverse(acc)
      case Cons(h, t) => loop(t, Cons(f(h), acc))
    }
    loop(as, Nil)
  }
}
