package akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class Master extends Actor {
  val serv = context.actorOf(Props(new Servant(self)))
  var ogSender: Option[ActorRef] = None
  var assigned = false
  def receive = {
    case "hey" =>
      if (!assigned) {
        ogSender = Some(sender)
        assigned = true
      }
      println("rcvd: hey")
      serv ! "reply"
    case "hello back" =>
      println("rcvd: hello back")
      ogSender map { a => a ! "got a reply" }
  }
}

class Servant(mstr: ActorRef) extends Actor {
  def receive = {
    case "reply" =>
      println("rcvd: reply")
      mstr ! "hello back"
  }
}

object BackAndForth extends App {
  implicit val timeout = Timeout(10 seconds)
  val system = ActorSystem("bnf-sys")
  val actor = system.actorOf(Props[Master])

  val future = actor ? "hey"

  future.onSuccess {
    case msg: String => println("finally: " + msg)
    case _   => println("bleh")
  }
}
