package akka.converse

/**
 * Created by burgosr on 7/15/14.
 */
sealed trait Event
case class Login(user: String) extends Event
case class Logout(user: String) extends Event
case class Message(from: String, message: String) extends Event
