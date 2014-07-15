package akka.converse

import akka.actor.{ActorRef, Actor}

/**
 * Created by burgosr on 7/15/14.
 */
class User(room: ActorRef) extends Actor {
  def receive = {
    case login @ Login(name) =>
      room ! login
    case logout @ Logout(name) =>
      room ! logout
    case message @ Message(from, msg) =>
      println(msg)
  }
}
