package sbt
package client

import java.io.Closeable
import concurrent.{ ExecutionContext, Future }
import play.api.libs.json.Writes

final class ChannelInUseException() extends Exception("This channel is already in use and can only be claimed once")

/**
 * SbtChannel is a "raw" connection to the sbt server which gives you the plain
 *  protocol without keeping track of or caching anything for you. Wrap it
 *  in SbtClient for a much more convenient API.
 */
trait SbtChannel extends Closeable {
  /** UUID of this sbt connection, different every time we connect. */
  def uuid: java.util.UUID
  /**
   * Name used to store configuration associated with this connection; usually
   *  the same machine-readable name every time the same app connects.
   */
  def configName: String
  /** Human-readable name of this client, such as the name of the app. */
  def humanReadableName: String

  /**
   * Send a message over the sbt socket. Normally these are from the sbt.protocol package.
   *  If we fail to write to the socket, the future gets an exception. Note that just because
   *  the future succeeds doesn't mean the server received and acted on the message.
   */
  def sendJson[T: Writes](message: T): Future[Unit]

  /**
   * Send a message over the sbt socket, getting the serial in a callback which allows you to
   *  provide a result based on the reply. The "registration" callback is run synchronously
   *  (before this method returns) and will always run before the message is sent.
   */
  def sendJsonWithRegistration[T: Writes, R](message: T)(registration: Long => Future[R]): Future[R]

  /**
   * Send a reply message (replyTo is the serial of the request we are replying to;
   * each request gets 0 or 1 replies, defined in the protocol for each kind of request.
   * If we fail to write to the socket, the future gets an exception. Note that just because
   * the future succeeds doesn't mean the server received and acted on the message.
   */
  def replyJson[T: Writes](replyTo: Long, message: T): Future[Unit]

  /**
   * Invoke a function in the given ExecutionContext for every message received over this channel.
   * This may be called ONLY ONCE by whoever will primarily use the channel; calling it claims the channel
   * and starts handling events. This avoids races on startup (we don't want to lose events before
   * a handler has been attached).
   * If this is called twice you will get ChannelInUseException. All channels need a "primary
   * owner" which controls when the stream of events starts and handles requests and such.
   *
   * The listener is guaranteed to get a ClosedEvent as the last message; if the channel is
   * already closed, it will be sent immediately (through the provided ExecutionContext).
   *
   * The provided serializations are used to decode task results.
   *
   * NOTE your ExecutionContext needs to keep messages in order or you will be sad!
   */
  def claimMessages(listener: protocol.Envelope => Unit, serializations: protocol.ReadOnlyDynamicSerialization)(implicit ex: ExecutionContext): Subscription

  /**
   * Like claimMessages but can be called more than once and does not start the message stream. No messages will be sent
   *  until someone does claimMessages().
   */
  def handleMessages(listener: protocol.Envelope => Unit)(implicit ex: ExecutionContext): Subscription

  /** true if close() has been called or the socket was closed by the server. */
  def isClosed: Boolean
}
