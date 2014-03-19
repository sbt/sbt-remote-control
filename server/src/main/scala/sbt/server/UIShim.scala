package sbt
package server

import play.api.libs.json.{ Format, JsValue }

private[server] class ServerUIContext(state: ServerState) extends AbstractUIContext {

  private def withClient[A](state: ServerState)(f: (WorkId, LiveClient) => A): Option[A] = {
    state.lastCommand match {
      // work.allRequesters.head picks a client at random to do the confirmation protocol.
      case Some(LastCommand(work)) => Some(f(work.id, work.allRequesters.head))
      case _ => None
    }
  }
  // TODO - this is probably bad
  private def waitForever[A](f: concurrent.Future[A]): A =
    concurrent.Await.result(f, concurrent.duration.Duration.Inf)

  // TODO - Figure out how to block on input from server
  def readLine(prompt: String, mask: Boolean): Option[String] =
    // TODO - Timeouts error handling....
    withClient(state) { (workId, client) =>
      waitForever(client.readLine(workId, prompt, mask))
    }.getOrElse(throw new java.io.IOException("No clients listening to readLine request."))
  def confirm(msg: String): Boolean =
    withClient(state) { (workId, client) =>
      System.out.println(s"Asking client($client) to confirm on request: $workId, $msg")
      waitForever(client.confirm(workId, msg))
      // TODO - Maybe we just always return some default value here.
    }.getOrElse(throw new java.io.IOException("No clients listening to confirm request."))

  def sendEvent[T: Format](event: T): Unit =
    state.eventListeners.send(event)
  def sendGenericEvent(data: JsValue): Unit =
    state.eventListeners.send(data)
}

object UIShims {

  private val uiContextSetting: Setting[_] =
    UIContext.uiContext in Global := {
      val state = sbt.Keys.state.value
      new ServerUIContext(ServerState.extract(state))
    }
  def makeShims(state: State): Seq[Setting[_]] =
    Seq(uiContextSetting)
}