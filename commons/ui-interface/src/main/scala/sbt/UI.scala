package sbt

import sbt.serialization._

sealed trait InteractionService {
  /** Prompts the user for input, optionally with a mask for characters. */
  def readLine(prompt: String, mask: Boolean): Option[String]
  /** Ask the user to confirm something (yes or no) before continuing. */
  def confirm(msg: String): Boolean

  // TODO - Ask for input with autocomplete?
}

sealed trait SendEventService {
  /** Sends an event out to all registered event listeners. */
  def sendEvent[T: SbtPickler](event: T): Unit
}

/**
 * Interface between tasks and jobs; tasks aren't allowed
 *  to directly mess with the BackgroundJob above. Methods
 *  on this interface should all be pure (conceptually this
 *  is immutable).
 */
sealed trait BackgroundJobHandle {
  def id: Long
  def humanReadableName: String
  def spawningTask: ScopedKey[_]
  // def tags: SomeType
}

sealed trait BackgroundJobService extends java.io.Closeable {

  /**
   * Launch a background job which is a function that runs inside another thread;
   *  killing the job will interrupt() the thread. If your thread blocks on a process,
   *  then you should get an InterruptedException while blocking on the process, and
   *  then you could process.destroy() for example.
   *
   *  TODO if we introduce a ServiceManager, we can pass that in to start instead of
   *  two hardcoded services.
   */
  def runInBackgroundThread(spawningTask: ScopedKey[_], start: (Logger, SendEventService) => Unit): BackgroundJobHandle

  def list(): Seq[BackgroundJobHandle]
  def stop(job: BackgroundJobHandle): Unit
  def waitFor(job: BackgroundJobHandle): Unit

  // TODO we aren't using this anymore; do we want it?
  def handleFormat: sbinary.Format[BackgroundJobHandle]
}

/**
 * Represents a Manifest/Serializer pair we can use
 *  to serialize task values + events later.
 */
sealed trait RegisteredSerializer {
  type T
  def manifest: Manifest[T]
  def serializer: SbtSerializer[T]
}
object RegisteredSerializer {
  def apply[U](s: SbtSerializer[U])(implicit mf: Manifest[U]): RegisteredSerializer =
    new RegisteredSerializer {
      type T = U
      override val serializer = s
      override val manifest = mf
    }
}
/**
 * Represents a dynamic type conversion to be applied
 * prior to selecting a RegisteredSerializer when sending over
 * the wire protocol.
 */
sealed trait RegisteredProtocolConversion {
  type From
  type To
  def fromManifest: Manifest[From]
  def toManifest: Manifest[To]
  def convert(from: From): To
}
object RegisteredProtocolConversion {
  def apply[F, T](convert: F => T)(implicit fromMf: Manifest[F], toMf: Manifest[T]): RegisteredProtocolConversion =
    new RegisteredProtocolConversion {
      override type From = F
      override type To = T
      override def fromManifest = fromMf
      override def toManifest = toMf
      override def convert(from: F): T = convert(from)
    }
}

object UIKeys {
  // TODO create a separate kind of key to lookup services separately from tasks
  val interactionService = taskKey[InteractionService]("Service used to ask for user input through the current user interface(s).")
  // TODO create a separate kind of key to lookup services separately from tasks
  val sendEventService = taskKey[SendEventService]("Service used to send events to the current user interface(s).")
  val registeredProtocolConversions = settingKey[Seq[RegisteredProtocolConversion]]("Conversions to apply before serializing task results.")
  val registeredSerializers = settingKey[Seq[RegisteredSerializer]]("All the serializers needed for task result values.")

  // jobService is a setting not a task because semantically it's required to always be the same one
  // TODO create a separate kind of key to lookup services separately from tasks
  val jobService = settingKey[BackgroundJobService]("Job manager used to run background jobs.")
  val jobList = taskKey[Seq[BackgroundJobHandle]]("List running background jobs.")
  val jobStop = inputKey[Unit]("Stop a background job by providing its ID.")
  val jobWaitFor = inputKey[Unit]("Wait for a background job to finish by providing its ID.")
  val backgroundRun = inputKey[BackgroundJobHandle]("Start an application's default main class as a background job")
  val backgroundRunMain = inputKey[BackgroundJobHandle]("Start a provided main class as a background job")
}
private[sbt] trait SbtPrivateInteractionService extends InteractionService
private[sbt] trait SbtPrivateSendEventService extends SendEventService
private[sbt] trait SbtPrivateBackgroundJobService extends BackgroundJobService
private[sbt] trait SbtPrivateBackgroundJobHandle extends BackgroundJobHandle
