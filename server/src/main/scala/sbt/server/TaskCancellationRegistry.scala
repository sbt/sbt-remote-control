package sbt
package server

import concurrent.{ Future, Promise, promise }
import java.util.concurrent.atomic.AtomicReference
import annotation.tailrec

trait TaskCancellationRegistry {
  /**
   * Register a new execution into the register for which
   *  we want to wait for cancellation notifications.
   *  @return a Future that will notify success upon cancellation.
   */
  def registerExecution(id: Long): Future[Unit]
  /**
   * Notify listeners that we want to cancel the execution with
   *  that has the given id.
   *
   *  @return  True if a cancellation request can be sent,
   *           false if the execution has already completed.
   */
  def cancelExecution(id: Long): Boolean
  /**
   * Notify the registry that the exeuction is done, so it
   *  should drop all data structures.
   */
  def doneExecution(id: Long): Unit
}

/**
 * Simple threadsafe implementation of the registry using AtomicReferences.
 */
class SimpleTaskCancellationRegistry extends TaskCancellationRegistry {
  private val registry: AtomicReference[Map[Long, Promise[Unit]]] = new AtomicReference(Map.empty)

  def registerExecution(id: Long): Future[Unit] = {
    // TODO - Error if promise already exists!
    @tailrec
    def updateStuff(): Future[Unit] = {
      val oldMap = registry.get
      val p = promise[Unit]
      val newMap = oldMap + (id -> p)
      if (registry.compareAndSet(oldMap, newMap)) p.future
      else updateStuff()
    }
    updateStuff()
  }

  def cancelExecution(id: Long): Boolean = {
    registry.get get id match {
      case Some(promise) =>
        promise.success(())
        true
      case None => false
    }
  }

  def doneExecution(id: Long): Unit = {
    @tailrec
    def doStuff(): Unit = {
      val oldMap = registry.get
      oldMap get id match {
        case Some(promise) =>
          // First clear the promise
          // TODO - We don't need a Stack trace here.
          promise.tryFailure(new RuntimeException("Completed without cancellation."))
          val newMap = oldMap - id
          if (registry.compareAndSet(oldMap, newMap)) ()
          else doStuff()
        case None => () // ignore, could be a thread issue
      }
    }
    doStuff
  }
}