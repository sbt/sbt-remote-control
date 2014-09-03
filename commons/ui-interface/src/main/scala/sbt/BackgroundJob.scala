package sbt

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
}

trait BackgroundJobManager extends java.io.Closeable {

  /**
   * Launch a background job which is a function that runs inside another thread;
   *  killing the job will interrupt() the thread. If your thread blocks on a process,
   *  then you should get an InterruptedException while blocking on the process, and
   *  then you could process.destroy() for example.
   */
  def runInBackgroundThread(spawningTask: ScopedKey[_], start: (Logger, SendEventService) => Unit): BackgroundJobHandle

  def list(): Seq[BackgroundJobHandle]
  def stop(job: BackgroundJobHandle): Unit
  def waitFor(job: BackgroundJobHandle): Unit

  def handleFormat: sbinary.Format[BackgroundJobHandle]
}

private[sbt] abstract class AbstractBackgroundJobManager extends BackgroundJobManager
