package sbt

import scala.util.control.NonFatal

/**
 * Interface between sbt and a thing running in the background.
 */
trait BackgroundJob {
  def humanReadableName: String
  def awaitTermination(): Unit
  def shutdown(): Unit
  // this should be true on construction and stay true until
  // the job is complete
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

  /**
   * Launch a background job which is a function that runs inside another thread;
   *  killing the job will interrupt() the thread. If your thread blocks on a process,
   *  then you should get an InterruptedException while blocking on the process, and
   *  then you could process.destroy() for example.
   */
  def runInBackgroundThread(streams: std.TaskStreams[ScopedKey[_]], start: (Logger, UIContext) => Unit): BackgroundJobHandle

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
}
