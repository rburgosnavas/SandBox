package akka

import akka.actor.{ActorSystem, Props, Actor}

/**
 * Created by burgosr on 6/16/14.
 */
object Basic extends App {

  class Hello extends Actor {

    override def preStart = {
      val greeter = context.actorOf(Props[Greeter], name = "Greeter")
      greeter ! Greeter.Greet
    }

    def receive = {
      case Greeter.Done =>
        println("hello done")
        context.stop(self)
    }

    @scala.throws[Exception](classOf[Exception])
    override def postStop() = {
      println("hello stopped")
    }

  }

  object Greeter {
    case object Greet
    case object Done
  }

  class Greeter extends Actor {

    def receive = {
      case Greeter.Greet =>
        println("Hi")
        sender ! Greeter.Done
    }

    @scala.throws[Exception](classOf[Exception])
    override def postStop() = {
      println("greeter stopped")
    }
  }

  val system = ActorSystem("Hello")
  val hello = system.actorOf(Props[Hello], name = "Hello")

}
