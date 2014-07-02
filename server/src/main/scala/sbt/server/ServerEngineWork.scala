package sbt
package server

import sbt.protocol._
import concurrent.{ Future, Promise }

final case class ExecutionId(id: Long) {
  require(id != 0L)
}

sealed trait ServerEngineWork {
  def id: ExecutionId
  /** True if this work has been finished. */
  def isCancelled: Boolean

}

/**
 * Or hook to exposing cancellation status (and cleanup)
 *  to the sbt build.
 */
trait WorkCancellationStatus {
  /** Run the given function if the work has been cancelled. */
  def onCancel(f: () => Unit): Unit
  /** Returns true if the work has been cancelled. */
  def isCancelled: Boolean
  /** Mark this work as completed (either success or failure). */
  def complete(): Unit
  /** Attempts to cancel this work. */
  def cancel(): Boolean
}
object WorkCancellationStatus {
  private object SameThreadExecutionContext extends concurrent.ExecutionContext {
    override def execute(r: Runnable): Unit = r.run()
    override def reportFailure(t: Throwable) = throw t
  }
  private object NotCancelled extends RuntimeException("Not Cancelled")
  /** constructs a new handler for cancel/complete notifications internally. */
  def apply(): WorkCancellationStatus =
    new WorkCancellationStatus {
      private val completer = Promise[Unit]
      override def onCancel(f: () => Unit): Unit =
        completer.future.foreach(_ => f())(SameThreadExecutionContext)
      override def isCancelled: Boolean = completer.isCompleted
      // Should clear out the registered listeners.
      override def complete(): Unit = completer.tryFailure(NotCancelled)
      override def cancel(): Boolean = completer.trySuccess(())
    }
}

// If you find yourself adding stuff from sbt.protocol such as the reply serial
// to this, you are doing it wrong because from here on out MULTIPLE clients
// not just the requester care about this work, so we don't want to special-case
// the original request anymore. We also combine requests into one of these
// chunks of work, thus allRequesters not a single requester.
/**
 * A case class representing a request for work to be performed.
 *
 * @param id - The id given to this request for execution.
 * @param command - The sbt command that is requested to be run
 * @param allRequesters - All clients associated with this request for work.
 * @param cancelRequest - A promise that will complete if cancel is requested.
 *                        If a cancel is never received, the future never completes.
 */
case class CommandExecutionWork(
  id: ExecutionId,
  command: String,
  allRequesters: Set[LiveClient],
  // TODO - Cancel handler thing should allow
  // saying that work is done as well as cancelling.
  cancelStatus: WorkCancellationStatus) extends ServerEngineWork {
  require(allRequesters.nonEmpty)
  /** Whether or not the work was cancelled. */
  def isCancelled: Boolean = cancelStatus.isCancelled
  /** Creates a new copy of this work, but with an additional client. */
  def withNewRequester(requester: LiveClient): CommandExecutionWork =
    CommandExecutionWork(id, command, allRequesters + requester, cancelStatus)
}
