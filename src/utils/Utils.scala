package utils

/**
 * Created by burgosr on 1/29/14.
 */
object Utils {
  type Width = Int
  type Height = Int

  /**
   *
   * @param f
   * @param z
   * @param l
   * @return
   */
  def fft(f: ((Int, Int), (Int, Int)) => (Int, Int))(z: (Int, Int))(l: List[(Int, Int)]): (Int, Int) = l match {
    case x::xs => fft(f)(f(z, x))(xs)
    case Nil   => z
  }

  /**
   *
   * @param l
   * @return
   */
  def rvrs(l: List[(Width, Height)]): List[(Width, Height)] = {
    def loop(curr: List[(Width, Height)], acc: List[(Width, Height)]): List[(Width, Height)] = curr match {
      case x::xs => loop(xs, x::acc)
      case Nil   => acc
    }
    loop(l, Nil)
  }

  /**
   *
   * @param l
   * @param f
   * @return
   */
  def mp(l: List[(Width, Height)])(f: ((Int, Int)) => (Int, Int)): List[(Width, Height)] = {
    def loop(curr: List[(Width, Height)], acc: List[(Width, Height)]): List[(Width, Height)] = curr match {
      case x::xs => loop(xs, f(x)::acc)
      case Nil   => rvrs(acc)
    }
    loop(l, Nil)
  }

  /**
   * Calculates and resizes dimension of a potential image.
   *
   * @param w img width
   * @param h img height
   * @param x amount to resize to
   * @param f function to resize image (+, -, * or /)
   * @return tuple with new dimensions
   */
  def rsz(w: Width, h: Height)(x: Int)(f: (Int, Int) => Int): (Width, Height) =
    (f(w, x), f(h, x))

  /**
   * Calculates and resizes dimension of a potential image against a list of
   * ranges.
   *
   * @param w img width
   * @param h img height
   * @param range a set of ranges
   * @param f function to resize image
   * @return a list of tuples with new dimension from range(0) to range(n)
   */
  def rszRng(w: Width, h: Height)(range: List[Int])(f: (Int, Int) => Int): List[(Width, Height)] =
    range.map(x => rsz(w, h)(x)(f))

  /**
   *
   * @param list
   * @param f
   * @return
   */
  def fltrLstTT(list: List[(Int, Int)])(f: ((Int, Int)) => Boolean): List[(Int, Int)] = {
    def loop(curr: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = curr match {
      case x::xs => if (f(x)) loop(xs, x::acc) else loop(xs, acc)
      case Nil   => rvrs(acc)
    }
    loop(list, Nil)
  }

}
