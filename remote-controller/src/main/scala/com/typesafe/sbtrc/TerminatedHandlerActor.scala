package com.typesafe.sbtrc

import akka.actor._

// this just lets us stack Terminated actions.
trait TerminatedHandlerActor extends Actor with ActorLogging {
  // when overriding, always chain up.
  def onTerminated(ref: ActorRef): Unit = {}
}
