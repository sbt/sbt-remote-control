package sbt
package server

import play.api.libs.json.Format
import concurrent.Future
/**
 * An interface we can use to send messages to an sbt client.
 *
 * TODO - better name!
 */
sealed trait SbtClient {
  /** Sends a message out to an sbt client.  This should be a safe call (doens't throw on bad client.) */
  def send[T: Format](msg: T): Unit

  /** Creates a new client that will send events to *both* of these clients. */
  def zip(other: SbtClient): SbtClient = (this, other) match {
    case (JoinedSbtClient(clients), JoinedSbtClient(clients2)) => JoinedSbtClient(clients ++ clients2)
    case (JoinedSbtClient(clients), other) => JoinedSbtClient(clients + other)
    case (other, JoinedSbtClient(clients2)) => JoinedSbtClient(clients2 + other)
    case (other, other2) => JoinedSbtClient(Set(other, other2))
  }
  // Removes a particular client from this potential aggregate client.
  def without(client: SbtClient): SbtClient =
    this match {
      case `client` | NullSbtClient => NullSbtClient
      case JoinedSbtClient(clients) if clients.contains(client) =>
        JoinedSbtClient(clients filterNot (_ == client))
      case other => other
    }
}
object NullSbtClient extends SbtClient {
  def send[T: Format](msg: T): Unit = ()
  override def toString = "NullSbtClient"
}
case class JoinedSbtClient(clients: Set[SbtClient]) extends SbtClient {
  // TODO - ignore individual failures?
  final def send[T: Format](msg: T): Unit =
    clients foreach (_ send msg)
  override def toString = clients.mkString("Joined(", ",", ")")
}
// This is what concrete implementations implement.
abstract class LiveClient extends SbtClient {
  /** requests a line of input from the client.  This will return sometime in the future. */
  def readLine(workId: WorkId, prompt: String, mask: Boolean): Future[Option[String]]
  /** Confirms a message from a client. */
  def confirm(workId: WorkId, msg: String): Future[Boolean]
  def reply[T: Format](replyTo: Long, msg: T): Unit
}

case class KeyValueClientListener[T](
  key: ScopedKey[T],
  client: SbtClient) {
  /** Disconnect a client from this listener. */
  def disconnect(c: SbtClient): KeyValueClientListener[T] =
    copy(client = client without c)

  def add(c: SbtClient): KeyValueClientListener[T] =
    copy(client = client zip c)
}

