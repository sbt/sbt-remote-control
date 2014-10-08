package sbt
package server

import play.api.libs.json.Writes
import sbt.protocol.BackgroundJobEvent
import sbt.protocol.TaskEvent
import sbt.protocol.Event
import sbt.AbstractBackgroundJobService
import sbt.protocol.BackgroundJobStarted
import sbt.protocol.BackgroundJobFinished
import sbt.protocol.CoreLogEvent
import sbt.protocol.LogMessage
import sbt.protocol.BackgroundJobInfo

private[server] class ServerInteractionService(state: ServerState) extends SbtPrivateInteractionService {

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

private[server] class TaskSendEventService(taskIdFinder: TaskIdFinder, eventSink: JsonSink[TaskEvent]) extends SbtPrivateSendEventService {

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
  extends SbtPrivateSendEventService {
  override def sendEvent[T: Writes](event: T): Unit =
    eventSink.send(BackgroundJobEvent(jobId, event))
}

private final class ServerBackgroundJobService(executionIdFinder: ExecutionIdFinder, logSink: JsonSink[protocol.LogEvent], eventSink: JsonSink[BackgroundJobEvent])
  extends AbstractBackgroundJobService {

  // synchronized access; store the execution ID of each job ID.
  // We don't want to use the executionIdFinder on job completion because
  // the execution may already be over.
  private var executionIds = Map.empty[Long, Long]

  protected override def makeContext(id: Long, spawningTask: ScopedKey[_]): (Logger with java.io.Closeable, SendEventService) = {
    val logger = new BackgroundJobEventLogger(id, logSink)
    val eventService = new BackgroundJobSendEventService(id, eventSink)
    (logger, eventService)
  }

  protected override def onAddJob(sendEventService: SendEventService, job: BackgroundJobHandle): Unit = {
    executionIdFinder.currentExecutionId map { executionId =>
      synchronized { executionIds += job.id -> executionId }
      sendEventService.sendEvent(BackgroundJobStarted(executionId,
        BackgroundJobInfo(id = job.id,
          humanReadableName = job.humanReadableName,
          spawningTask = SbtToProtocolUtils.scopedKeyToProtocol(job.spawningTask))))
    } getOrElse {
      logSink.send(CoreLogEvent(LogMessage(LogMessage.ERROR, s"Somehow we launched a job without an executionId ${job}")))
    }
  }
  protected override def onRemoveJob(sendEventService: SendEventService, job: BackgroundJobHandle): Unit = {
    synchronized { executionIds.get(job.id) } map { executionId =>
      sendEventService.sendEvent(BackgroundJobFinished(executionId = executionId, jobId = job.id))
      synchronized { executionIds -= job.id }
    } getOrElse {
      logSink.send(CoreLogEvent(LogMessage(LogMessage.ERROR, s"Somehow we ended a job with no recorded executionId ${job}")))
    }
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

  private def jobServiceSetting(executionIdFinder: ExecutionIdFinder, logSink: JsonSink[protocol.LogEvent], eventSink: JsonSink[BackgroundJobEvent]): Setting[_] =
    UIKeys.jobService := { new ServerBackgroundJobService(executionIdFinder, logSink, eventSink) }

  def makeShims(state: State, executionIdFinder: ExecutionIdFinder, taskIdFinder: TaskIdFinder, logSink: JsonSink[protocol.LogEvent], taskEventSink: JsonSink[TaskEvent], jobEventSink: JsonSink[BackgroundJobEvent]): Seq[Setting[_]] =
    Seq(
      UIKeys.registeredFormats in Global <<= (UIKeys.registeredFormats in Global) ?? Nil,
      UIKeys.registeredProtocolConversions in Global <<= (UIKeys.registeredProtocolConversions in Global) ?? Nil,
      jobServiceSetting(executionIdFinder, logSink, jobEventSink)) ++ uiServicesSettings(taskIdFinder, taskEventSink)
}
