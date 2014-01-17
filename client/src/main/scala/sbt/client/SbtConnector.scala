package sbt
package client

import java.io.Closeable
import concurrent.{ ExecutionContext, Future }

/**
 * This represents something that will connect to the sbt server *and* reconnect on failure.
 *
 * You can close the connection and stop reconnecting by calling `close()`.
 */
trait SbtConnector extends Closeable {
  /**
   * Register a callback to be notified on initial connection and subsequent reconnects to
   * an sbt server.  If the server is already connected, the handler will be called immediately.
   *
   * @param handler   A callback that will be called upon every connection/reconnect.
   * @param ex        The context (thread) where the handler should be executed.
   */
  def onConnect(handler: SbtClient => Unit)(implicit ex: ExecutionContext): Subscription
}