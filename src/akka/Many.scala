package akka

import akka.actor.{Props, ActorSystem, Actor}

/**
 * Created by burgosr on 6/16/14.
 */
object Many extends App {

  object Cmd {
    case object Greet
    case object Repeat
    case object SignOf
  }

  class Master extends Actor {

    val w1 = context.actorOf(Props[WorkerOne], "1")
    val w2 = context.actorOf(Props[WorkerTwo], "2")
    val w3 = context.actorOf(Props[WorkerThree], "3")

    def receive = {
      case "greet" => w1 ! Cmd.Greet
      case "repeat" => w2 ! Cmd.Repeat
      case "signoff" => w3 ! Cmd.SignOf
    }
  }

  class WorkerOne extends Actor {
    def receive = {
      case Cmd.Greet => println("hey")
    }
  }

  class WorkerTwo extends Actor {
    def receive = {
      case Cmd.Repeat =>
        for (i <- Range(0, 5)) {
          println("crazy!")
          Thread.sleep(5000)
        }
    }
  }

  class WorkerThree extends Actor {
    def receive = {
      case Cmd.SignOf => println("goodbye!")
    }
  }

  val system = ActorSystem("many")
  val master = system.actorOf(Props[Master], "master")
  val cmds = List("greet", "repeat", "signoff")

  cmds.foreach { master ! _ }

}
