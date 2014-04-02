package sbt
package client

import java.io.Closeable
import concurrent.{ ExecutionContext, Future }

/** This represents a connection to the Sbt server, and all the actions that can be performed against an active sbt build. */
trait SbtClient extends Closeable {

  def uuid: java.util.UUID
  def configName: String
  def humanReadableName: String

  /**
   * This is our mechanism of watching the build structure to see when it changes,
   * and update our information about the build.
   *
   * @param listener -  A listener that is notified on build structure changes.
   * @param ex - The context in which we should execute the listener.
   *
   * @return
   *      A subscription which can be used to unsubscribe to notifications.
   *
   * Note: This will load the entire build structure and pass it too us.
   */
  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription

  // TODO - A mechanism to dynamically load key-dependencies, which are too expensive to compute up front.
  // Possibly via watching a project and pulling configuration off of it.

  /**
   * Gives us the autocompletions possible for a given command string.
   *
   * @param partialCommand  An incomplete command or task string
   *
   * @return A set of "completion" strings that could be used as sbt commands.  These
   *         completions are triples of:
   *         - the string to append
   *         - the string to display to users
   *         - a flag denoting if the completion is empty.
   */
  def possibleAutocompletions(partialCommand: String, detailLevel: Int): Future[Set[Completion]]

  /**
   * This tries to find whether there is a build key associated with the
   * current string.  Returns all such keys (if aggregation is enabled).
   */
  def lookupScopedKey(name: String): Future[Seq[ScopedKey]]

  /**
   * Asks to run the command/task associated with the given input string.
   * The server has a queue of input strings to execute. When a string is
   * added to the queue, it gets an execution ID (returned from this method)
   * which will appear in related events. Events include ExecutionWaiting
   * when the execution is queued, ExecutionStarting when it is about to
   * run, and either ExecutionDone or ExecutionFailure when it is complete.
   *
   * Duplicates in the queue are combined. This means that if you requestExecution()
   * and the given command or task is already in the queue, you will get an
   * existing execution ID back, and there will not be a new ExecutionWaiting
   * event. Another implication of this is that execution requests may appear
   * to be re-ordered (since you can "jump ahead" in the queue if your request
   * is combined with one which is already present).
   *
   * @param commandOrTask The full command/task string to run.
   *         that should be evaluated.
   * @return  A future execution ID, which then appears in execution-related events
   */
  def requestExecution(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)]): Future[Long]

  /**
   * Adds a listener to general events which are fired from this sbt server.  These can be things like
   *  "TaskStarted, TaskCanceled, or even custom events from plugins (via GenericEvent).
   *
   *  @param listener  A function that is called when events are fired from sbt.
   *  @param ex        The execution context on which to call the listener.
   */
  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription
  /**
   * Adds a listener to a particular setting.  If this setting changes, the event listener
   *  will be notified with the new value (and sent the initial value).
   *
   *  @param key  The setting to listen to changes on.
   *  @param listener  A function that is called when the setting value changes.
   *  @param ex        The execution context on which to call the listener.
   */
  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription
  /**
   * Adds a listener for the value of a particular task.  If the evaluated task result changes, the event
   *  listener will be notified of the new value.
   *
   *  Since tasks read their state from the filesystem, it is not guaranteed that an event will be fired upon change *unless* some
   *  task in the dependency of the specified one is run, requiring the value of the current task to be re-evaluated.
   *
   *  @param key       The task to listen to changes for.
   *  @param listener  A function that is called when the setting value changes.
   *  @param ex        The execution context on which to call the listener.
   */
  def watch[T](key: TaskKey[T])(l: ValueListener[T])(implicit ex: ExecutionContext): Subscription
}