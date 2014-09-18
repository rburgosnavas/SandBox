package fpinscala.ch03

object ListRunner extends App {

  import fpinscala.ch03.List._

  println(sumTR(List(1,2,3)))
  println(s"productTR(List(20,10,3,10)) = ${productTR(List(20,0,3,10))}")

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
  println(s"\ndwhiled: ${dwhiled}")

  val appnded = append(List(1,2,8), List(3,4))
  println(s"\nappended: ${appnded}")

  // Ex. 6 --------------------------------------------------------------------
  val inits = init(l)
  println(s"\ninits: ${inits}")

  def add(x: Int, y: Int): Int = x + y
  val foldRSum = foldRight(List(1,2,3))(0)(add)
  println(s"\nfoldRSum: ${foldRSum}")

  // Ex. 8
  println(s"\n${foldRight(List(1,2,3))(Nil: List[Int])(Cons(_, _))}")

  println(s"\nlength(List(1,2,3,4,5,6,7,8,9): ${length(List(1,2,3,4,5,6,7,8,9))}")

  val foldLSum = foldLeft(List(1,2,3))(0)(add)
  println(s"\nfoldLSum: ${foldLSum}")

  val rev = reverse(List(1,2,3,4))
  println(s"\nreverse: ${rev}")

  // Ex. 13
  println(s"\n${foldRight2(List(1,2,3))(0)(add)}")

  // Ex. 14
  val appendedFold = appendFold(List(1,2,3), List(4,5,6))
  println(s"\nappendedFold: ${appendedFold}")

  // Ex. 15
  val concatd = concat(List(List(1,2), List(4,5), List(7,8)))
  println(s"\nconcatd: ${concatd}")

  // Ex. 16
  val incd = incIntList(List(1,2,3,4,5))
  println(s"\nincd: ${incd}")

  val mappedTR = mapTR(List(1,2,3,4,5))(_ + 1)
  println(s"\nmappedTR: ${mappedTR}")
}
