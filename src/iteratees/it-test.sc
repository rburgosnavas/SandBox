import scala.io.Source

val it = Source.fromFile("/home/burgosr/IdeaProjects/SandBox/src/iteratees/foo").getLines

def enum(it: Iterator[String]): Int = {
  var res = 0
  it foreach { l => res += l.toInt }
  res
}

val foo = enum(Source.fromFile("/home/burgosr/IdeaProjects/SandBox/src/iteratees/foo").getLines)
