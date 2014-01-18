package sbt
package server

import play.api.libs.json.{ Format, JsValue }

private[server] class ServerUIContext(state: ServerState) extends AbstractUIContext {
  // TODO - Figure out how to block on input from server
  def readLine(prompt: String, mask: Boolean): Option[String] = ???
  def confirm(msg: String): Boolean = ???

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