package sbt
package server

/**
 *  This represents the sycnhronization interface between the event loop
 *  and the command processing loop.
 */
trait ServerEngineQueue {
  /**
   * Grabs the next request + server state we should use in the main
   * sbt event loop.
   *
   * @param -  The most up-to-date state we should use when serving
   *           read-only requests.
   *
   * @return  A tuple containing:
   *          - The latest server state (event listeners and such)
   *          - The next request to execute.
   */
  def takeNextRequest: (ServerState, ServerRequest)
}