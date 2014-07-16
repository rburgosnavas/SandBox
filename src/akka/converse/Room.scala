package akka.converse

import akka.actor.{ActorRef, Actor}

import scala.collection.mutable.ListBuffer

/**
 * Created by burgosr on 7/15/14.
 */
class Room extends Actor {
  var list = ListBuffer.empty[ActorRef]

  def receive = {
    case Login(name) =>
      list += sender
    case Logout(name) =>
      list = list filter( _ != sender)
    case message @ Message(from, msg) =>
      list filter( _ != sender) foreach {
        _ ! message
      }
  }
}
