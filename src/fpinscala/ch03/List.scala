package fpinscala.ch03

/**
 * Created by burgosr on 6/19/14.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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
  //def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //  case Nil => l
  //  case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)
  //}
  // -- revised
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else Cons(x, xs)
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
}

object ListRunner extends App {

  import fpinscala.ch03.List._

  // Ex. 1 --------------------------------------------------------------------
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)

  // Ex. 4 --------------------------------------------------------------------
  val l = List(1,2,3,4,5)
  val dtail = tailDrop(l)
  println(dtail)

  // Ex. 5 --------------------------------------------------------------------
  // first version
  // val dwhiled = dropWhile(l, (x: Int) => x / 3 != 1)

  // revised
  val dwhiled = dropWhile(l)(x => x / 3 != 1)
  println(dwhiled)

  // Ex. 6 --------------------------------------------------------------------
  val inites = init(l)
  println(inites)
}
