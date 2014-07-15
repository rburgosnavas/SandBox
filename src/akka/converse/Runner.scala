package akka.converse

import java.util.Scanner

import akka.actor
import akka.actor.{Props, ActorSystem}

/**
 * Created by burgosr on 7/15/14.
 */
object Runner extends App {
  val system = ActorSystem("converse")
  val room = system.actorOf(Props[Room])
  val user = system.actorOf(Props(new User(room)))

  user ! Login("hi")
  val in = new Scanner(System.in)

  while(true) {
    print("> ")
    val msg = in.next
    room ! Message("", msg)
  }

  user ! Logout("bye")
}
