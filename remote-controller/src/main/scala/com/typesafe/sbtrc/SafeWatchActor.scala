package com.typesafe.sbtrc

import akka.actor._

// the intent of this is that if we need to watch/unwatch for multiple
// reasons, we don't have to worry about unwatching for one reason
// when we still needed to watch for a different reason.
// For example in ChildPool:
//  - if the same actor requests multiple sbts
//  - if the same actor requests an sbt and also watches events
trait SafeWatchActor extends TerminatedHandlerActor {

  private var watched = Map.empty[ActorRef, Int]

  final protected def watch(ref: ActorRef): Unit = {
    val oldCount = watched.get(ref).getOrElse(0)
    watched = watched + (ref -> (oldCount + 1))
    if (oldCount == 0)
      context.watch(ref)
  }

  final protected def unwatch(ref: ActorRef): Unit = {
    val oldCount = watched.get(ref).getOrElse(0)
    if (oldCount > 1) {
      watched = watched + (ref -> (oldCount - 1))
    } else if (oldCount == 1) {
      watched = watched - ref
      context.unwatch(ref)
    } else if (oldCount == 0) {
      // nothing
    }
  }

  override def onTerminated(ref: ActorRef): Unit = {
    super.onTerminated(ref)
    watched = watched - ref
  }
}
