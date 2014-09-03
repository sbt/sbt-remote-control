package sbt
package server

import play.api.libs.json.Writes
import sbt.protocol.BackgroundJobEvent
import sbt.protocol.TaskEvent
import sbt.protocol.Event
import sbt.protocol.DynamicSerialization
import sbt.BaseBackgroundJobManager

private[server] class ServerInteractionService(state: ServerState) extends AbstractInteractionService {

  private def withClient[A](state: ServerState)(f: (ExecutionId, LiveClient) => A): Option[A] = {
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
    withClient(state) { (executionId, client) =>
      waitForever(client.readLine(executionId, prompt, mask))
    }.getOrElse(throw new java.io.IOException("No clients listening to readLine request."))
  def confirm(msg: String): Boolean =
    withClient(state) { (executionId, client) =>
      System.out.println(s"Asking client($client) to confirm on request: $executionId, $msg")
      waitForever(client.confirm(executionId, msg))
      // TODO - Maybe we just always return some default value here.
    }.getOrElse(throw new java.io.IOException("No clients listening to confirm request."))
}

private[server] class TaskSendEventService(taskIdFinder: TaskIdFinder, eventSink: JsonSink[TaskEvent]) extends AbstractSendEventService {

  private def taskId: Long = {
    // TODO currently this depends on thread locals; we need to
    // set things up similar to how streams work now where we make
    // a per-task SendEventService which knows that task's ID. This may
    // involve changes to the sbt core.
    taskIdFinder.bestGuessTaskId(taskIfKnown = None)
  }

  override def sendEvent[T: Writes](event: T): Unit =
    eventSink.send(TaskEvent(taskId, event))
}

private final class BackgroundJobSendEventService(jobId: Long, eventSink: JsonSink[BackgroundJobEvent])
  extends AbstractSendEventService {
  override def sendEvent[T: Writes](event: T): Unit =
    eventSink.send(BackgroundJobEvent(jobId, event))
}

private final class ServerBackgroundJobManager(logSink: JsonSink[protocol.LogEvent], eventSink: JsonSink[BackgroundJobEvent])
  extends BaseBackgroundJobManager {

  protected override def makeContext(id: Long, spawningTask: ScopedKey[_]): (Logger with java.io.Closeable, SendEventService) = {
    val logger = new BackgroundJobEventLogger(id, logSink)
    val eventService = new BackgroundJobSendEventService(id, eventSink)
    (logger, eventService)
  }

  protected override def onAddJob(uiContext: SendEventService, job: BackgroundJobHandle): Unit = {
    // TODO to send the started event we need executionId to make it in here
  }
  protected override def onRemoveJob(uiContext: SendEventService, job: BackgroundJobHandle): Unit = {
    // TODO send the finished event
  }
}

object UIShims {

  private def uiServicesSettings(taskIdFinder: TaskIdFinder, eventSink: JsonSink[TaskEvent]): Seq[Setting[_]] = Seq(
    UIKeys.interactionService in Global := {
      val state = sbt.Keys.state.value
      new ServerInteractionService(ServerState.extract(state))
    },
    UIKeys.sendEventService in Global := {
      new TaskSendEventService(taskIdFinder, eventSink)
    })

  private def jobManagerSetting(logSink: JsonSink[protocol.LogEvent], eventSink: JsonSink[BackgroundJobEvent]): Setting[_] =
    UIKeys.jobManager := { new ServerBackgroundJobManager(logSink, eventSink) }

  def makeShims(state: State, taskIdFinder: TaskIdFinder, logSink: JsonSink[protocol.LogEvent], taskEventSink: JsonSink[TaskEvent], jobEventSink: JsonSink[BackgroundJobEvent]): Seq[Setting[_]] =
    Seq(
      UIKeys.registeredFormats in Global <<= (UIKeys.registeredFormats in Global) ?? Nil,
      jobManagerSetting(logSink, jobEventSink)) ++ uiServicesSettings(taskIdFinder, taskEventSink)
}
