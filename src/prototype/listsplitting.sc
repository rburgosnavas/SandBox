val imap = List(1 -> "IB", 2 -> "IB", 3 -> "IB", 4 -> "IB", 5 -> "OB", 6 -> "OB", 7 -> "OB")


imap.foreach {
  m => m match {
    case (x, y) if (y == "IB") => println(y)
  }
}







