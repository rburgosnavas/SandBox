package fpinscala.ch05

import java.util.Date

/**
 * Created by burgosr on 3/31/15.
 */
sealed trait Stream[+A] {
  import Stream._

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Ex 5.1
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  // Ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>  if (n > 0) Cons(h, () => t().take(n - 1)) else Empty
  }

  // Ex 5.2
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) Cons(h, t) else t().drop(n - 1)
  }

  // Ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // Ex 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // Ex 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else Empty
    }
  }

  // Ex 5.6
  def headOption2: Option[A] = {
    foldRight(None: Option[A]){ (a, b) =>
      val a_ = a
      Some(a_)
    }
  }

  // Ex 5.7
  def map[B >: A](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
  }

  // Ex 5.7
  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]){(a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else b
    }
  }

  // Ex 5.7
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => Cons(() => a, () => b))
  }

  // Ex 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
  }

  def map2[B >: A](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take2(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) =>
        if (n > 0) Some(h(), (t(), n - 1))
        else None
      case _ => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, s) if (p(h())) => Some((h(), s()))
      case _ => None
    }
  }

  def zipWith[B, C](st2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, st2)) {
      case (Cons(h1, s1), Cons(h2, s2)) =>
        Some(f(h1(), h2()), (s1(), s2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h1, s1), Cons(h2, s2)) => Some((Some(h1()), Some(h2())), (s1(), s2()))
      case (Cons(h1, s1), _) => Some((Some(h1()), None), (s1(), empty[B]))
      case (_, Cons(h2, s2)) => Some((None, Some(h2())), (empty[A], s2()))
      case _ => None
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case s @ Cons(h, t) => Some(s, t())
      case _ => None
    } append(empty)
  }

  def startWith[A](s: Stream[A]): Boolean = {
    // * zipAll the streams first to Stream[(Option, Option)]
    // * takeWhile from Stream[(Option, Option)] until the second Option in the
    //   tuple is None (isEmpty)
    //   This guarantees that we do not match a Some with a None, since that
    //   will yield `false`
    // * call forAll to check if all the first Option equals the second Option
    //   in the tuple
    zipAll(s) takeWhile {
      case (o1, o2) => !o2.isEmpty
    } forAll {
      case (o1, o2) => o1 == o2
    }
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
    ???
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  // Ex 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a, s)) => Cons(() => a, () => unfold(s)(f))
    }
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))
}

object StreamRunner extends App {
  import Stream._

  val st1 = Stream(1,2,3,4,5,6,7,8,9,10)
  val emp = empty
  println(st1.headOption)
  println(emp.headOption)

  // Ex 5.1
  println("\nEx 5.1")
  println(st1.toList)

  // Ex 5.2
  println("\nEx 5.2")
  println(st1.take(4).toList)
  println(st1.drop(4).toList)

  // Ex 5.3
  println("\nEx 5.3")
  println(st1.takeWhile(_ % 2 == 0).toList)

  println(st1.exists(_ == 11))
  println(st1.exists(_ == 7))
  println(st1.exists2(_ == 11))
  println(st1.exists2(_ == 7))

  // Ex 5.4
  println("\nEx 5.4")
  println(st1.forAll(_ < 20))
  println(st1.forAll(_ < 5))
  println(st1.forAll(_ % 2 == 0))
  println(st1.forAll(n => ((n * 3) % 3) == 0))

  // Ex 5.5
  println("\nEx 5.5")
  println(st1.takeWhile2(_ % 2 == 0).toList)

  // Ex 5.6
  println("\nEx 5.6")
  println(st1.headOption2)
  println(emp.headOption2)

  // Ex 5.7
  println("\nEx 5.7")
  println(st1.map(_ * 100).toList)
  println(st1.filter(_ % 2 == 0).toList)
  println(st1 filter(_ % 2 == 0) takeWhile2(_ < 10) map(_ * 10) toList)
  println(st1 append(Stream(11, 12, 13, 14, 15)) toList)
  println

  println(st1.map(Stream(_) toList) toList)
  println(st1.flatMap(Stream(_)) toList)

  // Ex 5.8
  println("\nEx 5.8")
  println(constant(new Date).take(10).toList)

  // Ex 5.9
  println("\nEx 5.9")
  println(from(0).take(10).toList)

  // Ex 5.11 & 5.12
  println("\nEx 5.11 & 5.12")
  println(from2(0).take(10).toList)

  def ff(n: Int): Option[(Int, Int)] =
    if (n < 20) Some(n, n + 1)
    else None

  println(unfold(0)(ff).toList)

  println(st1.take2(6).toList)
  println(st1.takeWhile3(_ < 5).toList)

  println(st1.take(3).zipWith(st1.take(3))(_ * _).toList)
  println(st1.take(7).zipAll(st1.take(3)).toList)

  println(st1.tails.toList)

  println(st1.zipAll(from2(1).take2(2)).takeWhile {
    case (o1, o2) => !o2.isEmpty
  }.toList)
  println(st1.startWith(from2(1).take(2)))

  println(Stream(1,2,3).scanRight(0)(_ + _).toList)
}
