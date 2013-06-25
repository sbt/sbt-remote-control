package com.typesafe.sbtrc

import akka.actor._

// if you derive from this you need to handle Terminated and
// chain up to onTerminated. There's no nice way to mixin
// behaviors to receive()
trait EventSourceActor extends SafeWatchActor {

  private var subscribers = Map.empty[ActorRef, Int]

  final protected def subscribe(ref: ActorRef): Unit = {
    val oldCount = subscribers.get(ref).getOrElse(0)
    subscribers = subscribers + (ref -> (oldCount + 1))
    if (oldCount == 0)
      watch(ref)
  }

  final protected def isSubscribed(ref: ActorRef): Boolean = {
    subscribers.contains(ref)
  }

  final protected def unsubscribe(ref: ActorRef): Unit = {
    val oldCount = subscribers.get(ref).getOrElse(0)
    if (oldCount > 1) {
      subscribers = subscribers + (ref -> (oldCount - 1))
    } else if (oldCount == 1) {
      subscribers = subscribers - ref
      unwatch(ref)
    } else if (oldCount == 0) {
      // nothing
    }
  }

  final protected def emitEvent[T](event: T): Unit = {
    for (s <- subscribers.keys) {
      s ! event
    }
    if (subscribers.isEmpty)
      log.debug("event had no listeners: {}", event)
  }

  private var freezeCount = 0
  // this needs to lazy-eval "event" _after_ body
  final protected def emitEventIfOutermost[T, R](event: => T)(body: => R): R = {
    freezeCount += 1
    val result = try {
      body
    } finally {
      freezeCount -= 1
    }
    if (freezeCount == 0)
      emitEvent(event)

    result
  }

  override def onTerminated(ref: ActorRef): Unit = {
    super.onTerminated(ref)
    unsubscribe(ref)
  }
}
