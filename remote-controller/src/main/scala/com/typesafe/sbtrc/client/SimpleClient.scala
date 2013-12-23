package com.typesafe.sbtrc
package client

import protocol._
import api._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import java.net.SocketException
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Very terrible implementation of the sbt client.
 *
 *
 * Not only is it blockin', we have mutability hell and lockin'.
 *
 * This is only to do proof of concept work and flesh out the server.
 */
class SimpleSbtClient(client: ipc.Client, closeHandler: () => Unit) extends SbtClient {

  private val listeningToEvents = new AtomicBoolean(false)

  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription = ???
  def possibleAutocompletions(partialCommand: String): Future[Set[String]] = ???
  def lookupScopedKey(name: String): Future[Option[ScopedKey]] = ???

  def requestExecution(commandOrTask: String): Future[Unit] = {
    val result = Promise[Unit]()
    client.sendJson(ExecutionRequest(commandOrTask))
    // TODO - Figure out how notify the ACK response...
    result.success(())
    result.future
  }
  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription = {
    val helper = new EventListenerHelper(listener, ex)
    addEventListenerImpl(helper)
    object subscription extends Subscription {
      def cancel(): Unit =
        removeEventListenerImpl(helper)
    }
    if (listeningToEvents.compareAndSet(false, true)) {
      client.sendJson(ListenToEvents())
    }
    subscription
  }
  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription = ???
  def watch[T](key: TaskKey[T])(l: ValueListener[T])(implicit ex: ExecutionContext): Subscription = ???

  // TODO - Implement
  def close(): Unit = {
    running = false
    thread.join()
  }

  @volatile var running = false
  @volatile var listeners: Set[EventListenerHelper] = Set.empty
  private def addEventListenerImpl(l: EventListenerHelper): Unit = synchronized {
    listeners += l
  }
  private def removeEventListenerImpl(l: EventListenerHelper): Unit = synchronized {
    listeners -= l
  }
  private def sendEvent(e: Event): Unit =
    listeners foreach (_ send e)

  // TODO - Error handling!
  object thread extends Thread {
    override def run(): Unit = {
      while (running) {
        try handleNextEvent()
        catch {
          case e: SocketException =>
            running = false
        }
      }
      closeHandler()
    }
    def handleNextEvent(): Unit =
      protocol.Envelope(client.receive()) match {
        // TODO - Filter events, like build change events vs. normal events...
        case protocol.Envelope(_, _, e: Event) => sendEvent(e)
        // TODO - Deal with other responses...
        case _ =>
      }
  }
  thread.start()
}

/** A wrapped event listener that ensures events are fired on the desired exeuction context. */
private[client] class EventListenerHelper(listener: EventListener, ex: ExecutionContext) {
  val id = java.util.UUID.randomUUID
  def send(e: Event): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e)
      }
    })
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: EventListenerHelper => x.id == id
    case _ => false
  }

}