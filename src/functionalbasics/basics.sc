/**
 * Created by burgosr on 3/28/14.
 */

def $[A, B, C](f: B => C)(g: A => B): (A => C) = (a:A) => f(g(a))

def foldr[A, B](f: A => B => B)(z: B)(l: List[A]): B = l match {
  case Nil => z
  case x::xs => foldr(f)(f(x)(z))(xs)
}

foldr((x:Int) => (y:Int) => (x * y))(1)(List(1,2,3,4,5))

def rev[A](l: List[A]): List[A] =
  foldr((x:A) => (xs:List[A]) => x::xs)(Nil:List[A])(l)

rev(List(1,2,3,4,5))

def fltr[A](f: A => Boolean)(l: List[A]): List[A] =
  rev(foldr((x:A) => (xs:List[A]) => if (f(x)) x::xs else xs)(Nil:List[A])(l))

fltr((x: Int) => x % 2 == 0)(List(1,2,3,4,5))
