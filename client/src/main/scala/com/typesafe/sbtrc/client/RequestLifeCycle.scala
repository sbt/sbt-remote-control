package com.typesafe.sbtrc
package client

import sbt.client.Interaction
import concurrent.ExecutionContext
import java.io.Closeable

/** Handles events during a request's lifecycle. */
private[client] class RequestLifecycle(val serial: Long, val interaction: Interaction) {
  private val receivedPromise = concurrent.promise[Unit]
  val received = receivedPromise.future
  def error(msg: String): Unit =
    receivedPromise.failure(new RequestException(msg))
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
      def run(): Unit = {
        try result.success(f)
        catch {
          case NonFatal(e) => result.failure(e)
        }
      }
    })
    Await.result(result.future, Inf)
  }

}

private[client] class ExecutionNotFound(executionId: Long) extends Exception(s"Execution $executionId not found")

private[client] class RequestHandler extends Closeable {
  // requests are initially tracked by serial, then converted into
  // by execution ID once we get one.
  // Multiple requests may have the same execution ID,
  // serials are unique. Errors by serial are protocol breakage
  // while errors after that should be sbt errors.
  private var bySerial: Map[Long, RequestLifecycle] = Map.empty
  private var byExecutionId: Map[Long, Seq[RequestLifecycle]] = Map.empty
  override def close(): Unit = {
    for (lifecycle <- bySerial.values)
      lifecycle.error("Connection to sbt closed")
    bySerial = Map.empty
  }
  def register(serial: Long, interaction: Option[(Interaction, ExecutionContext)] = None): RequestLifecycle = {
    synchronized {
      val (iact, context) = interaction match {
        case Some((i, c)) => (i, c)
        case None => (NoInteraction, ExecutionContext.global)
      }
      val lifecycle = new RequestLifecycle(serial, new InteractionHelper(iact, context))
      bySerial += serial -> lifecycle
      lifecycle
    }
  }
  def readLine(executionId: Long, prompt: String, mask: Boolean): Option[String] =
    withExecution(executionId) { request =>
      request.interaction.readLine(prompt, mask)
    }
  def confirm(executionId: Long, message: String): Boolean =
    withExecution(executionId) { request =>
      request.interaction.confirm(message)
    }
  def protocolError(requestSerial: Long, msg: String): Unit = {
    finishRequest(requestSerial, executionIdOption = None)(_.error(msg))
  }
  def executionReceived(requestSerial: Long, executionId: Long): Unit = {
    finishRequest(requestSerial, Some(executionId))(_.accepted())
  }
  def executionDone(executionId: Long): Unit = {
    finishExecutions(executionId)(identity)
  }
  def executionFailed(executionId: Long, message: String): Unit = {
    // TODO we just swallow the error! that can't be quite right.
    executionDone(executionId)
  }

  private def withExecution[A](executionId: Long)(f: RequestLifecycle => A): A =
    synchronized {
      // we pick a request with this executionId at random, if
      // there are multiple
      byExecutionId get executionId flatMap (_.headOption) match {
        case Some(req) => f(req)
        case None => throw new ExecutionNotFound(executionId)
      }
    }
  private def finishExecutions(executionId: Long)(f: Seq[RequestLifecycle] => Unit): Unit = {
    synchronized {
      byExecutionId get executionId match {
        case Some(reqs) =>
          try f(reqs)
          finally byExecutionId -= executionId
        case None => // TODO - Issue some error or log!
      }
    }
  }
  private def finishRequest(serial: Long, executionIdOption: Option[Long])(f: RequestLifecycle => Unit): Unit = {
    synchronized {
      bySerial get serial match {
        case Some(req) =>
          // move it to byExecutionId
          for (executionId <- executionIdOption) {
            byExecutionId += executionId -> (byExecutionId.get(executionId) map { existing =>
              req +: existing
            } getOrElse {
              Seq(req)
            })
          }
          // run function and remove from bySerial
          try f(req)
          finally bySerial -= serial
        case None => // TODO - Issue some error or log!
      }
    }
  }
}