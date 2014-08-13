package sbt
package server

/**
 *  This represents the synchronization interface between the event loop
 *  and the command processing loop.
 */
trait ServerEngineQueue {
  /**
   * Blocks until we have a new request.
   *
   * Note:  This will ensure that the work has already been minimized
   *        in the queue before responding.
   */
  def blockAndTakeNext: (RequestListeners, ServerEngineWork)
}