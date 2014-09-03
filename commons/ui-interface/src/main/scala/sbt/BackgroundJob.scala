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

  // TODO drop this to keep the interface pure
  override def toString = s"BackgroundJobHandle(${id},${humanReadableName},${Def.showFullKey(spawningTask)})"
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

  // TODO drop these to keep interface pure
  final def stop(id: Long): Unit =
    list().find(_.id == id).foreach(stop(_))
  final def waitFor(id: Long): Unit =
    list().find(_.id == id).foreach(waitFor(_))
}

private[sbt] abstract class AbstractBackgroundJobManager extends BackgroundJobManager
