package fpinscala.ch03

import Tree._

object TreeRunner extends App {
  val t1 =
    Branch(
      Branch(
        Leaf("yeah"),
        Branch(
          Leaf("yeah"),
          Leaf("ok"))),
      Branch(
        Leaf("yeah"),
        Leaf("ok")
      )
    )

  val t2 =
    Branch(
      Branch(
        Leaf(5000),
        Leaf(200)),
      Leaf(9)
    )

  println(s"t1 = ${t1}")
  println(s"\nsize(t1): ${size(t1)}")
  println(s"\nsize(t2): ${size(t2)}")
  println(s"\nmaximum(t2): ${maximum(t2)}")
  println(s"\ndepth(t1): ${depth(t1)}")
  println(s"\ndepth(t2): ${depth(t2)}")
  println(s"\nmap(t1): ${map(t1)(_.toUpperCase)}")
  println(s"\nmap(t2): ${map(t2)(_ + 100)}")
  println(s"\nfold(t2)(0)((x, y) => x + y): ${fold(t2)(0)(x => y => x + y)}")
}
