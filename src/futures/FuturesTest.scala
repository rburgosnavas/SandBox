package futures

import javax.net.ssl.SSLHandshakeException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.io.Source

/**
 * Created by burgosr on 6/13/14.
 */
object FuturesTest extends App {

  val f: Future[String] = Future {
    Source.fromURL("https://stroll.nextbus.com/" +
      "api/admin/v1/agencies/jta/predefined-messages").mkString
  }

  f onSuccess {
    case t => println(t)
  }

  f onFailure {
    case t: SSLHandshakeException => println(t)
  }

  Thread.sleep(1000 * 10)

}
