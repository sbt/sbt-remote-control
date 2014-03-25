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
   * Begin trying to connect to the server. Handlers may be called multiple times
   * if we disconnect and then reconnect. If a connection is already active when you call
   * this, your onConnect handler will be called immediately. If the connector
   * has already been closed when you call this, your onError handler will be called immediately.
   * Otherwise the handlers are called when connection or error occurs.
   *
   * The onConnect handler is invoked for initial connection and each subsequent successful
   * reconnect.
   *
   * The onError handler is invoked anytime we fail to connect or anytime the connection
   * is closed. The boolean parameter is true if we will try to connect again and false
   * if we are permanently closed. The string parameter is the error message.
   *
   * Both handlers are run in the provided execution context.
   *
   * The returned subscription may be canceled to remove both handlers. The subscription
   * will also be canceled when the SbtConnector is closed.
   */
  def open(onConnect: SbtClient => Unit, onError: (Boolean, String) => Unit)(implicit ex: ExecutionContext): Subscription
}