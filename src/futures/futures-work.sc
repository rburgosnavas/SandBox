import javax.net.ssl.SSLHandshakeException

import scala.concurrent._
import scala.io.{Codec, Source}
import ExecutionContext.Implicits.global

val f: Future[String] = future {
  Source.fromURL("https://stroll.nextbus.com/api/admin/v1/agencies/jta/predefined-messages/").mkString
}

f onSuccess {
  case t => println(t)
}

f onFailure {
  case t: SSLHandshakeException => println(t)
}
