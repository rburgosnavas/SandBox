package fpinscala.ch04

import scala.{Option => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  // flatMap is the same as >>= in Haskell
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  // flatMap2 is the same as >>= in Haskell
  def flatMap2[B](f: A => Option[B]): Option[B] = {
    // map(f) returns an Option[Option[B]]
    // getOrElse returns an Option[B]
    map(f) getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] =  {
    this flatMap { v =>
      if (f(v)) Some(v)
      else None
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    ???
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // extract 'aa: A' from 'a: Option[A]'
    a flatMap2 { aa =>
      // extract 'bb: B' from 'b: Option[B]'
      b map { bb =>
        // pass 'aa' and 'bb' to 'f'
        // returns 'C'
        f(aa, bb)
      }
    }
  }

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])
                      (f: (A, B, C) => D): Option[D] = {
    a flatMap2 { aa =>
      b flatMap2 { bb =>
        c map { cc =>
          f(aa, bb, cc)
        }
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (v => sequence(t) map (v :: _))
  }
}
