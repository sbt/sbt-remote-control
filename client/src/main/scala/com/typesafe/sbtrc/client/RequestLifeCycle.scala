package com.typesafe.sbtrc
package client

import sbt.client.Interaction
import concurrent.ExecutionContext

/** Handles events during a request's lifecycle. */
private[client] class RequestLifecycle(val serial: Long, val interaction: Interaction) {
  private val receivedPromise = concurrent.promise[Unit]
  val received = receivedPromise.future
  def error(msg: String): Unit = {
    receivedPromise.failure(new RequestException(msg))
  }
  def accepted(): Unit =
    receivedPromise.success(())
}
// TODO - Throw some sort of exception or other means of denoting we do not support interaction!
private[client] object NoInteraction extends Interaction {
  def readLine(prompt: String, mask: Boolean): Option[String] = throw NoInteractionException
  def confirm(msg: String): Boolean = throw NoInteractionException
}
object NoInteractionException extends Exception("No user interaction is defined for this request!")
// Turns async execution context calls into blocking on the client event thread.
private[client] class InteractionHelper(i: Interaction, ex: ExecutionContext) extends Interaction {
  import concurrent.{ Await, promise }
  import concurrent.duration.Duration.Inf
  import util.control.NonFatal
  def readLine(prompt: String, mask: Boolean): Option[String] =
    withExecutionContext(i.readLine(prompt, mask))
  def confirm(msg: String): Boolean =
    withExecutionContext(i.confirm(msg))
  private def withExecutionContext[A](f: => A): A = {
    val result = promise[A]
    ex.prepare.execute(new Runnable {
      def run(): Unit =
        try result.success(f)
        catch {
          case NonFatal(e) => result.failure(e)
        }
    })
    Await.result(result.future, Inf)
  }

}

private[client] class RequestNotFound(serial: Long) extends Exception(s"Request $serial not found")

private[client] class RequestHandler {
  private var runningRequests: Map[Long, RequestLifecycle] = Map.empty
  def register(serial: Long, interaction: Option[(Interaction, ExecutionContext)] = None): RequestLifecycle = {
    synchronized {
      val (iact, context) = interaction match {
        case Some((i, c)) => (i, c)
        case None => (NoInteraction, ExecutionContext.global)
      }
      val lifecycle = new RequestLifecycle(serial, new InteractionHelper(iact, context))
      runningRequests += serial -> lifecycle
      lifecycle
    }
  }
  def readLine(serial: Long, prompt: String, mask: Boolean): Option[String] =
    withRequest(serial) { request =>
      request.interaction.readLine(prompt, mask)
    }
  def confirm(serial: Long, message: String): Boolean =
    withRequest(serial) { request =>
      request.interaction.confirm(message)
    }
  def error(serial: Long, msg: String): Unit =
    finishRequest(serial)(_.error(msg))
  def accepted(serial: Long): Unit =
    try withRequest(serial)(_.accepted())
    catch {
      case e: RequestNotFound =>
    }
  def completed(serial: Long): Unit =
    finishRequest(serial)(identity)

  private def withRequest[A](serial: Long)(f: RequestLifecycle => A): A =
    synchronized {
      runningRequests get serial match {
        case Some(req) => f(req)
        case None => throw new RequestNotFound(serial)
      }
    }
  private def finishRequest(serial: Long)(f: RequestLifecycle => Unit): Unit = {
    synchronized {
      runningRequests get serial match {
        case Some(req) =>
          try f(req)
          finally runningRequests -= serial
        case None => // TODO - Issue some error or log!
      }
    }
  }
}