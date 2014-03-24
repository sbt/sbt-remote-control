package sbt
package server

/**
 *  This represents the synchronization interface between the event loop
 *  and the command processing loop.
 */
trait ServerEngineQueue {
  /**
   * Gets EITHER a new request to enqueue, or if none, the current
   * ServerState which should be used to process the queue.
   * This is sort of an odd method signature but it enforces that
   * we don't touch ServerState at any old time, only when
   * we should be about to execute commands. We can only
   * execute commands after draining the request queue.
   */
  def pollNextRequest: Either[ServerState, ServerRequest]
  /**
   * Blocks until we have a new request. You can't
   * get ServerState here because you still need to poll to
   * see if there are more requests to be consolidated,
   * before getting ServerState and then finally executing
   * one of the requests.
   */
  def takeNextRequest: ServerRequest
}
