package sbt
package client

import java.io.Closeable
import concurrent.{ ExecutionContext, Future }

/**
 * This represents something that will connect to the sbt server *and* reconnect on failure.
 *
 * Start trying to connect by calling open().
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

  /**
   * Register a callback to be notified anytime we disconnect or have some other error.
   * Callback parameters are a boolean "reconnecting" and an error message.
   * If an error is fatal/permanent or the connector has been closed, reconnecting
   * will be false. Otherwise if we're going to retry (or have never tried) reconnecting
   * will be true.
   *
   * @param handler A callback invoked on errors; if we are permanently closed,
   *                the boolean is false; string is the error message.
   */
  def onError(handler: (Boolean, String) => Unit)(implicit ex: ExecutionContext): Subscription

  /**
   * Begin trying to connect to the server. Set up callbacks prior to opening the
   * connection, your callbacks will receive any errors or created clients.
   * If called twice, subsequent calls will force immediate retry if we are currently
   * in a retry timeout; if we're currently connected then later calls do nothing.
   */
  def open(): Unit
}