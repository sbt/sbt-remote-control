package sbt

import scala.util.control.NonFatal

/**
 * Interface between sbt and a thing running in the background.
 */
trait BackgroundJob {
  def humanReadableName: String
  def awaitTermination(): Unit
  def shutdown(): Unit
  def isRunning(): Boolean
  // called after stop or on spontaneous exit, closing the result
  // removes the listener
  def onStop(listener: () => Unit)(implicit ex: concurrent.ExecutionContext): java.io.Closeable
  // do we need this or is the spawning task good enough?
  // def tags: SomeType
}

/**
 * Interface between tasks and jobs; tasks aren't allowed
 *  to directly mess with the BackgroundJob above. Methods
 *  on this interface should all be pure (conceptually this
 *  is immutable).
 */
trait BackgroundJobHandle {
  def id: Long
  def humanReadableName: String
  def spawningTask: ScopedKey[_]
  // def tags: SomeType

  override def toString = s"BackgroundJobHandle(${id},${humanReadableName},${Def.showFullKey(spawningTask)})"
}

trait BackgroundJobManager extends java.io.Closeable {

  // TODO An unsolved issue here is where these Logger and
  // UIContext come from, and how we can make this extensible
  // rather than two hardcoded things. So one way to make
  // it extensible might be to have a task which returns
  // a BackgroundJob, and we generate special streams
  // for that task or something. Or maybe hardcoding these
  // two things will be simplest. Not sure.
  // The streams instance is passed in because we can use it
  // to figure out which task is calling runInBackground.
  def runInBackground(streams: std.TaskStreams[ScopedKey[_]], start: (Logger, UIContext) => BackgroundJob): BackgroundJobHandle

  def list(): Seq[BackgroundJobHandle]
  def stop(job: BackgroundJobHandle): Unit
  def waitFor(job: BackgroundJobHandle): Unit
  final def stop(id: Long): Unit =
    list().find(_.id == id).foreach(stop(_))
  final def waitFor(id: Long): Unit =
    list().find(_.id == id).foreach(waitFor(_))

}

object BackgroundJob {
  // this is a setting not a task because semantically it's required to always be the same one
  val jobManager = settingKey[BackgroundJobManager]("Job manager used to run background jobs.")
  val jobList = taskKey[Seq[BackgroundJobHandle]]("List running background jobs.")
  val jobStop = inputKey[Unit]("Stop a background job by providing its ID.")
  val jobStopBySpawningTask = inputKey[Unit]("Stop all background jobs with the given spawning task.")
  val jobWaitFor = inputKey[Unit]("Wait for a background job to finish by providing its ID.")
  val backgroundRun = inputKey[BackgroundJobHandle]("Start an application's default main class as a background job")
  val backgroundRunMain = inputKey[BackgroundJobHandle]("Start a provided main class as a background job")

  private val nextThreadId = new java.util.concurrent.atomic.AtomicInteger(1)

  private class BackgroundThread(val taskName: String, body: () => Unit) extends Thread(s"sbt-bg-$taskName-${nextThreadId.getAndIncrement}") {
    setDaemon(true)

    override def run() = try body()
    finally cleanup()

    private class StopListener(val callback: () => Unit, val executionContext: concurrent.ExecutionContext) extends java.io.Closeable {
      override def close(): Unit = removeListener(this)
      override def hashCode: Int = System.identityHashCode(this)
      override def equals(other: Any): Boolean = other match {
        case r: AnyRef => this eq r
        case _ => false
      }
    }

    // access is synchronized
    private var stopListeners = Set.empty[StopListener]

    private def removeListener(listener: StopListener): Unit = synchronized {
      stopListeners -= listener
    }

    def onStop(listener: () => Unit)(implicit ex: concurrent.ExecutionContext): java.io.Closeable = synchronized {
      val result = new StopListener(listener, ex)
      stopListeners += result
      result
    }

    def cleanup(): Unit = {
      // avoid holding any lock while invoking callbacks, and
      // handle callbacks being added by other callbacks, just
      // to be all fancy.
      while (synchronized { stopListeners.nonEmpty }) {
        val listeners = synchronized {
          val list = stopListeners.toList
          stopListeners = Set.empty
          list
        }
        listeners.foreach { l =>
          l.executionContext.prepare().execute(new Runnable { override def run = l.callback() })
        }
      }
    }
  }

  private class BackgroundJobThread(thread: BackgroundThread) extends BackgroundJob {
    def awaitTermination(): Unit = thread.join()
    def humanReadableName: String = thread.taskName
    def isRunning(): Boolean = thread.isAlive()
    def shutdown(): Unit = thread.interrupt()

    def onStop(listener: () => Unit)(implicit ex: concurrent.ExecutionContext): java.io.Closeable = thread.onStop(listener)
  }

  /**
   * Launch a background job which is a function that runs inside another thread;
   *  killing the job will interrupt() the thread. If your thread blocks on a process,
   *  then you should get an InterruptedException while blocking on the process, and
   *  then you could process.destroy() for example.
   */
  def startBackgroundThread(manager: BackgroundJobManager, streams: std.TaskStreams[ScopedKey[_]])(work: (Logger, UIContext) => Unit): BackgroundJobHandle = {
    // TODO this gets me "compile:backgroundRun::streams" but I really want "compile:backgroundRun" -
    // not sure what the magic incantation is.
    val taskName = streams.key.scope.task.toOption.map(_.label).getOrElse("<unknown task>")
    def start(logger: Logger, uiContext: UIContext): BackgroundJob = {
      val thread = new BackgroundThread(taskName, { () =>
        work(logger, uiContext)
      })
      thread.start()
      new BackgroundJobThread(thread)
    }

    manager.runInBackground(streams, start)
  }
}
