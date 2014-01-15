package com.typesafe.sbtrc
package client

import protocol._
import api._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import java.net.SocketException
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.NonFatal
import java.io.IOException
import java.io.EOFException

/**
 * Very terrible implementation of the sbt client.
 *
 *
 * Not only is it blockin', we have mutability hell and lockin'.
 *
 * This is only to do proof of concept work and flesh out the server.
 */
class SimpleSbtClient(client: ipc.Client, closeHandler: () => Unit) extends SbtClient {

  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription =
    buildEventManager.watch(listener)(ex)

  def possibleAutocompletions(partialCommand: String): Future[Set[String]] = ???
  def lookupScopedKey(name: String): Future[Option[ScopedKey]] = ???

  def requestExecution(commandOrTask: String): Future[Unit] = {
    val result = Promise[Unit]()
    client.sendJson(ExecutionRequest(commandOrTask))
    // TODO - Figure out how notify the ACK response...
    result.success(())
    result.future
  }
  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription =
    eventManager.watch(listener)(ex)
  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription =
    valueEventManager[T](key.key).watch(listener)(ex)
  def watch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription =
    valueEventManager[T](key.key).watch(listener)(ex)

  // TODO - Implement
  def close(): Unit = {
    running = false
    thread.join()
  }

  @volatile var running = true
  private object eventManager extends ListenerManager[protocol.Event, EventListener, ListenToEvents](ListenToEvents(), client) {
    override def wrapListener(l: EventListener, ex: ExecutionContext) = new EventListenerHelper(l, ex)
  }
  private object buildEventManager extends ListenerManager[MinimalBuildStructure, BuildStructureListener, ListenToBuildChange](ListenToBuildChange(), client) {
    override def wrapListener(l: BuildStructureListener, ex: ExecutionContext) = new BuildListenerHelper(l, ex)
  }
  private object valueEventManager {
    private var valueListeners = collection.mutable.Map.empty[ScopedKey, ValueChangeManager[_]]

    def apply[T](key: ScopedKey): ValueChangeManager[T] = synchronized {
      // Yes, we cheat types here...
      valueListeners.get(key) match {
        case Some(mgr) => mgr.asInstanceOf[ValueChangeManager[T]]
        case None =>
          val mgr =
            new ValueChangeManager[Any](key, client).asInstanceOf[ValueChangeManager[T]]
          System.err.println("Adding event manager for key: " + key)
          valueListeners.put(key, mgr)
          mgr
      }
    }
  }

  // TODO - Error handling!
  object thread extends Thread {
    override def run(): Unit = {
      while (running) {
        try handleNextEvent()
        catch {
          case e @ (_: SocketException | _: EOFException) =>
            e.printStackTrace()
            running = false
          case e: IOException =>
            e.printStackTrace()
        }
      }
      // Here we think sbt connection has closed.
      client.close()
      closeHandler()
    }
    def handleNextEvent(): Unit =
      protocol.Envelope(client.receive()) match {
        case protocol.Envelope(_, _, e: ValueChange[_]) =>
          System.err.println("Value Change Event: " + e)
          valueEventManager(e.key).sendEvent(e)
        case protocol.Envelope(_, _, e: BuildStructureChanged) =>
          buildEventManager.sendEvent(e.structure)
        case protocol.Envelope(_, _, e: Event) =>
          eventManager.sendEvent(e)
        // TODO - Deal with other responses...
        case stuff =>
        // TODO - Do something here.
        //System.err.println("Received gunk from the server!: " + stuff)
      }
  }
  thread.start()
}

/** Abstracted mechanism of sending events. */
trait ListenerType[Event] {
  def send(e: Event): Unit
}
/** Helper to manage registering events and telling the server we want them. */
private abstract class ListenerManager[Event, Listener, RequestMsg <: Request](requestEventsMsg: RequestMsg, client: ipc.Peer)(implicit jsonFormat: ipc.JsonWriter[RequestMsg]) {

  def wrapListener(l: Listener, ex: ExecutionContext): ListenerType[Event]

  private val listeningToEvents = new AtomicBoolean(false)
  private var listeners: Set[ListenerType[Event]] = Set.empty

  def watch(listener: Listener)(implicit ex: ExecutionContext): Subscription = {
    val helper = wrapListener(listener, ex)
    addEventListener(helper)
    object subscription extends Subscription {
      def cancel(): Unit =
        removeEventListener(helper)
    }
    if (listeningToEvents.compareAndSet(false, true)) {
      client.sendJson(requestEventsMsg)
    }
    subscription
  }

  private def addEventListener(l: ListenerType[Event]): Unit = synchronized {
    listeners += l
  }
  private def removeEventListener(l: ListenerType[Event]): Unit = synchronized {
    listeners -= l
  }
  def sendEvent(e: Event): Unit = synchronized {
    listeners foreach { l =>
      try l send e
      catch {
        case NonFatal(_) => // Ignore non fatal exceptions while sending events.
      }
    }
  }
}

/** A wrapped event listener that ensures events are fired on the desired execution context. */
private[client] class EventListenerHelper(listener: EventListener, ex: ExecutionContext) extends ListenerType[Event] {
  private val id = java.util.UUID.randomUUID
  override def send(e: Event): Unit = {
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
/** A wrapped build event listener that ensures events are fired on the desired execution context. */
private[client] class BuildListenerHelper(listener: BuildStructureListener, ex: ExecutionContext) extends ListenerType[MinimalBuildStructure] {
  private val id = java.util.UUID.randomUUID
  override def send(e: MinimalBuildStructure): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e)
      }
    })
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: BuildListenerHelper => x.id == id
    case _ => false
  }
}

/** A wrapped build event listener that ensures events are fired on the desired execution context. */
private[client] class ValueChangeListenerHelper[T](listener: ValueListener[T], ex: ExecutionContext) extends ListenerType[ValueChange[T]] {
  private val id = java.util.UUID.randomUUID
  override def send(e: ValueChange[T]): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e.key, e.value)
      }
    })
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: ValueChangeListenerHelper[_] => x.id == id
    case _ => false
  }
}
/** Helper to track value changes. */
private final class ValueChangeManager[T](key: ScopedKey, peer: ipc.Peer)
  extends ListenerManager[ValueChange[T], ValueListener[T], ListenToValue](ListenToValue(key), peer) {

  def wrapListener(l: ValueListener[T], ex: ExecutionContext): ListenerType[ValueChange[T]] =
    new ValueChangeListenerHelper(l, ex)
}