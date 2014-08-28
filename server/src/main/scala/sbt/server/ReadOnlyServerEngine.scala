package sbt
package server

import java.util.Queue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean
import sbt.protocol._
import scala.annotation.tailrec
import java.util.concurrent.LinkedBlockingQueue
import play.api.libs.json.{ JsValue, Writes }
import scala.util.control.NonFatal

sealed trait SocketMessage

// a little wrapper around protocol.request to keep the client/serial with it
case class ServerRequest(client: LiveClient, serial: Long, request: protocol.Request) extends SocketMessage

case object SocketClosed extends SocketMessage

/**
 * This class represents an event loop which sits outside the normal sbt command
 * processing loop.
 *
 * This event loop can handle ServerState requests which are read-only against build state
 * or affect only the server state.   All other requests (mostly just ExecutionRequest) are added into a command queue
 * which the Command processing loop is expected to consume.
 *
 * @param queue - The Queue from which we read server requests and handle them.
 * @param nextStateRef - The atomic reference we use to communicate the current build state between ourselves and the
 *                       full server engine.
 *
 */
class ReadOnlyServerEngine(
  queue: BlockingQueue[SocketMessage],
  nextStateRef: AtomicReference[State]) extends Thread("read-only-sbt-event-loop") {
  private val running = new AtomicBoolean(true)
  // TODO - We should probably limit the number of deferred client requests so we don't explode during startup to DoS attacks...
  private val deferredStartupBuffer = collection.mutable.ArrayBuffer.empty[ServerRequest]

  // These listeners are handed off to ServerEngine with
  // each request so ServerEngine doesn't see mid-request
  // changes to the listeners.
  // This var should only be written by the request-queue-handling thread,
  // but gets read by the ServerEngine thread.
  // So what we care about is that when we handle a listener change request,
  // then next pass a request to ServerEngine, ServerEngine should see
  // the results of the listener change request. volatile should do that.
  @volatile private var requestListeners = TransformableRequestListeners()

  /**
   * This object is used by the ServerEngine to send
   *  out events. Methods can be called from any thread.
   *  The reason we need this abstraction vs. using
   *  serverState.eventListeners directly is that we
   *  want to "catch up" clients who were not connected
   *  and missed key events, including boot logs before
   *  any client connects, or execution status events,
   *  for example.
   */
  final object eventSink extends JsonSink[Event] {
    // Create ambiguity to keep us from using any accidental implicit
    // formatters; we need to always use the formatter we are given,
    // not replace it with our own.
    implicit def ambiguousWrites: Writes[Any] = ???
    implicit def ambiguousWrites2 = ambiguousWrites

    // the idea of this buffer is to just hold on to logs
    // whenever we have no clients at all, so we make a best
    // effort to ensure at least one client gets each log event.
    private val bufferedLogs = new LinkedBlockingQueue[EventWithWrites[protocol.CoreLogEvent]]()
    private def drainBufferedLogs(client: SbtClient): Unit = {
      @tailrec
      def drainAnother(): Unit = {
        Option(bufferedLogs.poll()) match {
          case Some(event) =>
            clientSendWithWrites(client, event)
            drainAnother()
          case None =>
        }
      }
      drainAnother()
    }

    // these require synchronization together; if we add a listener,
    // it needs to atomically 1) get current state and 2) be added to
    // listeners, otherwise it could get the wrong state with respect
    // to which events it gets.
    private var executionEngineState: ImpliedState.ExecutionEngine =
      ImpliedState.ExecutionEngine.empty
    private var eventListeners: SbtClient = NullSbtClient

    override def send[T <: Event](msg: T)(implicit writes: Writes[T]): Unit = {
      msg match {
        case event: CoreLogEvent =>
          eventListeners match {
            case NullSbtClient =>
              bufferedLogs.add(EventWithWrites.withWrites(event)(writes.asInstanceOf[Writes[CoreLogEvent]]))
            case client: SbtClient =>
              drainBufferedLogs(client)
              client.send(msg)(writes)
          }
        case event: ExecutionEngineEvent =>
          executionEngineState = ImpliedState.processEvent(executionEngineState, event)
          eventListeners.send(msg)(writes)
        case event: ExecutionWaiting =>
          executionEngineState = ImpliedState.processEvent(executionEngineState, event)
          eventListeners.send(msg)(writes)
        case other =>
          eventListeners.send(msg)(writes)
      }
    }

    private def clientSendWithWrites[E <: Event](client: SbtClient, withWrites: EventWithWrites[E]): Unit = {
      client.send(withWrites.event)(withWrites.writes)
    }

    private def sendActiveExecutionState(client: LiveClient): Unit = synchronized {
      val events = ImpliedState.eventsToReachEngineState(executionEngineState)
      events foreach { withWrites => clientSendWithWrites(client, withWrites) }
    }

    def addEventListener(l: LiveClient): Unit = synchronized {
      drainBufferedLogs(l)
      sendActiveExecutionState(l)
      val next = eventListeners zip l
      eventListeners = next
    }
    def removeEventListener(l: LiveClient): Unit = synchronized {
      val next = eventListeners without l
      eventListeners = next
    }
    def disconnect(client: SbtClient): Unit = synchronized {
      eventListeners = eventListeners without client
    }
  }

  /**
   * Object we use to synch work between the read-only "fast" event loop and
   *  the ServerEngine.
   *
   *  This is an ugly synchronized animal living between two threads.  There are
   *  two method called from the ReadOnlySide:
   *     - enqueueWork:  Push a new client request into the queue, joining with existing requests
   *     - cancelRequest: Attempt to cancel a request in the queue, or notify non-blocking to the ServerEngine.
   *  And one method called from the ServerEngine side
   *     - blockAndTakeNext: Block the current thread until some work is ready to run, and give me the latest state.
   *
   */
  final object engineWorkQueue extends ServerEngineQueue {

    private var nextExecutionId: Long = 1L // 1 is first, because 0 is invalid flag.
    private def getAndIncrementExecutionId: Long = {
      val prev = nextExecutionId
      nextExecutionId += 1
      prev
    }
    private var workQueue: List[ServerEngineWork] = Nil
    private var cancelStore: Map[Long, WorkCancellationStatus] = Map.empty

    override def toString = synchronized {
      "engineWorkQueue(" + workQueue + ")"
    }

    /**
     * Called by the ServerEngine thread.  This should block that thread
     *  until work is ready, then notify that the work is starting and
     *  grab the current serverState.
     */
    override def blockAndTakeNext: (RequestListeners, ServerEngineWork) =
      synchronized {
        // Block until we have work ready
        def blockUntilWork(): ServerEngineWork =
          workQueue match {
            case hd :: tail =>
              workQueue = tail
              hd
            case _ =>
              // Here we need to block until we have more work
              this.wait() // Note: waiting on this, means the queue has more data.
              blockUntilWork()
          }
        val work = blockUntilWork()

        // we do this here so it's synchronized (work is either in-queue or started)
        work match {
          case c: CommandExecutionWork => eventSink.send(protocol.ExecutionStarting(c.id.id))
          case EndOfWork =>
        }

        (requestListeners, work)
      }

    def close(): Unit = synchronized {
      workQueue = workQueue :+ EndOfWork
      notify()
    }

    /**
     * Called from the ReadOnlyEngine thread.
     *
     *  This is responsible for minimizing work on the way in,
     *  handling cancellations, etc.
     */
    def enqueueWork(work: ServerRequest): Unit =
      synchronized {
        // Check to see if something similar is on the queue
        // -  If so, merge the two together
        // -  If not:
        //       Notify current listeners that work is pending
        //       Add to the queue
        work match {
          case ServerRequest(client, serial, command: ExecutionRequest) =>
            val (work, isNew) =
              workQueue find {
                case old: CommandExecutionWork => old.command == command.command
                case _ => false
              } match {
                case Some(previous: CommandExecutionWork) =>
                  previous.withNewRequester(client) -> false
                case Some(other) => // just to fix exhaustiveness warning
                  throw new RuntimeException(s"impossible case, we should have filtered out $other")
                case None =>
                  // Make a new work item
                  val id = ExecutionId(getAndIncrementExecutionId)
                  // Register cancellation notifications.
                  val cancel = WorkCancellationStatus()
                  cancelStore += (id.id -> cancel)
                  // Create work item
                  val newItem = CommandExecutionWork(id, command.command, Set(client), cancel)
                  newItem -> true
              }
            // Always notify the current client of his work. serial 0L means synthetic in-server
            // execution request with no client originating it.
            if (serial != 0L) {
              import sbt.protocol.executionReceivedFormat
              client.reply(serial, ExecutionRequestReceived(id = work.id.id))
            }
            if (isNew) {
              // If this is a new item in the queue, tell all clients about it.
              eventSink.send(protocol.ExecutionWaiting(work.id.id, work.command, client.info))
            }
            // Now, we insert the work either at the end, or where it belongs.
            def insertWork(remaining: List[ServerEngineWork]): List[ServerEngineWork] =
              remaining match {
                case hd :: tail => hd match {
                  case cmd: CommandExecutionWork if cmd.id == work.id =>
                    work :: tail
                  case other =>
                    other :: insertWork(tail)
                }
                case Nil => work :: Nil
              }
            workQueue = insertWork(workQueue)
            // Here we should notify the server about the new work!
            notify()
          case wtf =>
            // TODO - Server/Logic error
            throw new Error(s"Square peg in round hole!  Is not a workRequest item: $wtf")
        }
      }

    def cancelRequest(id: Long): Boolean =
      synchronized {
        // Find out if we have a work item with the given id.
        // - If so, mark it as starting/failed in two events, and remove it. return true
        // - If not, try to cancel something in the store
        // - otherwise, fail.
        val found = workQueue collect {
          case old: CommandExecutionWork if old.id.id == id => old
        }
        found match {
          case item :: Nil =>
            // Remove the item from the queue
            workQueue = workQueue collect {
              case old: CommandExecutionWork if old.id.id != id => old
            }
            // Tell everyone that this request is dead.
            eventSink.send(protocol.ExecutionStarting(id))
            eventSink.send(protocol.ExecutionFailure(id))
            // mark it as cancelled (this removes it from our store)
            removeCancelStore(id)
            item.cancelStatus.cancel()

          case _ =>
            cancelStore get id match {
              case Some(value) =>
                removeCancelStore(id)
                value.cancel()
              case _ => false
            }
        }
      }

    private def removeCancelStore(id: Long): Unit =
      cancelStore -= id
  }

  // this is private because we only call it ourselves when we get SocketClosed;
  // other people who might call this should probably be stopping the socket
  // handler instead.
  private def quit(): Unit = if (running.get) {
    engineWorkQueue.close()
    running.set(false)
  }

  override def run() {
    // we buffer most requests until 1) we have a state and 2) the project loads successfully.

    while (running.get && Option(nextStateRef.get).map(!Project.isProjectLoaded(_)).getOrElse(true)) {
      @tailrec
      def drainRequests(): Unit = {
        // don't check running.get again in here! we need to drain everything.
        // we have to poll here because we want to continue even without
        // a request, if we get a state. Keep the poll short to avoid a needless
        // lag on startup.
        queue.poll(100, java.util.concurrent.TimeUnit.MILLISECONDS) match {
          case null => () // no more requests for now
          case SocketClosed => quit() // sets running=false
          case ServerRequest(client, serial, request) =>
            handleRequestsNoBuildState(client, serial, request)
            // see if there are more requests
            drainRequests()
        }
      }

      // get all pending requests - we need to drain the queue entirely
      // before we check running.get again, because for example a kill server
      // request might set running.get to false, but we still want to
      // handle requests up until the server shuts down.
      drainRequests()
    }

    // Now we flush through all the events we buffered, unless we've already
    // been shut down without ever loading the project.
    if (running.get) {
      for {
        ServerRequest(client, serial, request) <- deferredStartupBuffer
      } handleRequestsWithBuildState(client, serial, request, nextStateRef.get)
    }

    // Now we just run with the initialized build.
    while (running.get) {
      // Here we can block on requests, because we have cached
      // build state and no longer have to see if the sbt command
      // loop is started.
      queue.take match {
        case ServerRequest(client, serial, request) =>
          try handleRequestsWithBuildState(client, serial, request, nextStateRef.get)
          catch {
            case NonFatal(e) =>
              client.reply(serial, protocol.ErrorResponse(e.getMessage))
          }
        case SocketClosed =>
          quit()
      }
    }
  }

  private def syntheticExecuteRequest(client: LiveClient, serial: Long, scopedKey: sbt.ScopedKey[_], buildState: State): Unit = {
    handleRequestsWithBuildState(client, serial, ExecutionRequest(Def.showFullKey(scopedKey)), buildState)
  }
  private def listenToEvents(client: LiveClient, serial: Long): Unit = {
    eventSink.addEventListener(client)
    client.reply(serial, ReceivedResponse())
  }
  private def unlistenToEvents(client: LiveClient, serial: Long): Unit = {
    eventSink.removeEventListener(client)
    client.reply(serial, ReceivedResponse())
  }
  private def handleRequestsNoBuildState(client: LiveClient, serial: Long, request: Request): Unit =
    request match {

      //// If you change any of these, you probably also need to change
      //// handleRequestsWithBuildState below.

      case KillServerRequest() =>
        quit()
      case ListenToEvents() =>
        listenToEvents(client, serial)
      case UnlistenToEvents() =>
        unlistenToEvents(client, serial)
      case ClientClosedRequest() =>
        requestListeners = requestListeners.disconnect(client)
        client.reply(serial, ReceivedResponse())

      //// Here we don't want to buffer a bunch up because the user
      //// is probably waiting on completions and if we reply to a flood
      //// of these on project load the UI will get wonky. When the
      //// project loads they can always press tab again or whatever.
      case CommandCompletionsRequest(line, level) =>
        client.reply(serial, CommandCompletionsResponse(results = Set.empty))

      case _ =>
        // Defer all other messages....
        deferredStartupBuffer.append(ServerRequest(client, serial, request))
    }
  private def handleRequestsWithBuildState(client: LiveClient, serial: Long, request: Request, buildState: State): Unit =
    request match {
      //// First, requests we also handle without build state... hard to factor out
      //// without losing the match exhaustiveness warnings. If you change
      //// these change above too.

      case KillServerRequest() =>
        quit()
      case ListenToEvents() =>
        listenToEvents(client, serial)
      case UnlistenToEvents() =>
        unlistenToEvents(client, serial)
      case ClientClosedRequest() =>
        requestListeners = requestListeners.disconnect(client)
        client.reply(serial, ReceivedResponse())

      //// Second, requests we only handle when we have state,
      //// or handle differently when we have state.

      case ListenToBuildChange() =>
        requestListeners = requestListeners.addBuildListener(client)
        client.reply(serial, ReceivedResponse())
      case UnlistenToBuildChange() =>
        requestListeners = requestListeners.removeBuildListener(client)
        client.reply(serial, ReceivedResponse())
      case SendSyntheticBuildChanged() =>
        BuildStructureCache.sendBuildStructure(client, SbtDiscovery.buildStructure(buildState))
      case KeyLookupRequest(key) =>
        client.reply(serial, KeyLookupResponse(key, keyLookup(buildState, key)))
      case AnalyzeExecutionRequest(command) =>
        client.reply(serial, AnalyzeExecutionResponse(analyzeExecution(buildState, command)))
      case ListenToValue(key) =>
        SbtToProtocolUtils.protocolToScopedKey(key, buildState) match {
          case Some(scopedKey) =>
            val extracted = Project.extract(buildState)

            if (SettingCompletions.isSetting(scopedKey.key)) {
              // get the value of the setting key from the build state, and send it to the client
              val settingKey = SettingKey(scopedKey.key.asInstanceOf[sbt.AttributeKey[Any]]) in scopedKey.scope
              val change = SbtToProtocolUtils.settingKeyToProtocolValue(settingKey, buildState, extracted)
              client.send(ValueChanged(key, change))

              // register the key listener.
              // TODO: needs support somewhere to send events when the value of setting keys are updated
              requestListeners = requestListeners.addKeyListener(client, scopedKey)
            } else {
              // Schedule the key to run as well as registering the key listener.
              requestListeners = requestListeners.addKeyListener(client, scopedKey)
            }
            client.reply(serial, ReceivedResponse())

          case None => // Issue a no such key error
            client.reply(serial, KeyNotFound(key))
        }
      case UnlistenToValue(key) =>
        SbtToProtocolUtils.protocolToScopedKey(key, buildState) match {
          case Some(scopedKey) =>
            requestListeners = requestListeners.removeKeyListener(client, scopedKey)
            client.reply(serial, ReceivedResponse())
          case None => // Issue a no such key error
            client.reply(serial, KeyNotFound(key))
        }
      case SendSyntheticValueChanged(key) =>
        SbtToProtocolUtils.protocolToScopedKey(key, buildState) match {
          case Some(scopedKey) =>
            val extracted = Project.extract(buildState)

            if (SettingCompletions.isSetting(scopedKey.key)) {
              // get the value of the setting key from the build state, and send it to the client
              val settingKey = SettingKey(scopedKey.key.asInstanceOf[sbt.AttributeKey[Any]]) in scopedKey.scope
              val change = SbtToProtocolUtils.settingKeyToProtocolValue(settingKey, buildState, extracted)
              client.send(ValueChanged(key, change))
            } else {
              // for tasks, we have to run the task to generate the change event.
              // we set serial=0 because we don't want to generate a reply for the
              // synthetic ExecutionRequest generated in this call.

              // FIXME TODO BROKEN this should guarantee a ValueChanged EVEN IF the
              // value has not really changed, and should send it ONLY to this
              // client requesting a synthetic change if it hasn't actually changed.
              // Right now this will generate a ValueChanged even on no change only
              // because we are broken and always send it when we run the task...
              // but if we fix that, then this won't be guaranteed to send an event
              // at all.
              syntheticExecuteRequest(client, serial = 0L, scopedKey, buildState)
            }
            client.reply(serial, ReceivedResponse())

          case None => // Issue a no such key error
            client.reply(serial, KeyNotFound(key))
        }
      case req: ExecutionRequest =>
        // TODO - Handle "queue is full" issues.
        engineWorkQueue.enqueueWork(ServerRequest(client, serial, request))
      case CancelExecutionRequest(id) =>
        client.reply(serial, CancelExecutionResponse(engineWorkQueue.cancelRequest(id)))
      // don't client.reply to ExecutionRequest here - it's done in the work queue
      case keyRequest: KeyExecutionRequest =>
        // translate to a regular ExecutionRequest
        SbtToProtocolUtils.protocolToScopedKey(keyRequest.key, buildState) match {
          case Some(scopedKey) =>
            syntheticExecuteRequest(client, serial, scopedKey, buildState)
          case None =>
            client.reply(serial, KeyNotFound(keyRequest.key))
        }
      case CommandCompletionsRequest(line, level) =>
        val combined = buildState.combinedParser
        val completions = complete.Parser.completions(combined, line, level)
        def convertCompletion(c: complete.Completion): protocol.Completion =
          protocol.Completion(
            c.append,
            c.display,
            c.isEmpty)
        client.reply(serial, CommandCompletionsResponse(completions.get map convertCompletion))
      case _: ConfirmRequest | _: ReadLineRequest =>
        client.reply(serial, ErrorResponse(s"Request ${request.getClass.getName} is intended to go from server to client"))
      case _: RegisterClientRequest =>
        client.reply(serial, ErrorResponse("Client can only be registered once, on connection"))
    }

  private def keyLookup(buildState: State, key: String): Seq[protocol.ScopedKey] = {
    val parser: complete.Parser[Seq[sbt.ScopedKey[_]]] = Act.aggregatedKeyParser(buildState)
    import SbtToProtocolUtils.scopedKeyToProtocol
    complete.Parser.parse(key, parser) match {
      case Right(sk) => sk.map(k => scopedKeyToProtocol(k))
      case Left(msg) => Seq.empty
    }
  }

  private def analyzeExecution(buildState: State, command: String): ExecutionAnalysis = {
    // use the same parser as sbt.Command.process to determine whether
    // or not the execution will be parsed; but then we don't get back
    // enough info to tell how it's truly interpreted, so we have to
    // then try to guess
    val parser = Command.combine(buildState.definedCommands)
    complete.DefaultParsers.parse(command, parser(buildState)) match {
      case Right(runCommand) =>
        // here comes the heuristic part
        val keys = keyLookup(buildState, command)
        if (keys.isEmpty) {
          val commands: Seq[SimpleCommand] = buildState.definedCommands.collect {
            case c: SimpleCommand if (command == c.name || command.startsWith(s"${c.name} ")) => c
          }
          commands.headOption map { c =>
            ExecutionAnalysisCommand(name = Some(c.name))
          } getOrElse {
            // this is a command, probably, but we can't figure out which one
            ExecutionAnalysisCommand(name = None)
          }
        } else {
          // this is SLIGHTLY oversimplified, in that sbt can still throw an
          // error after this point if you try to mix input and regular tasks
          // or maybe in some other cases. But a task can always fail, too, so
          // I think we can just ignore those cases. We'll see I suppose.
          ExecutionAnalysisKey(keys)
        }
      case Left(error) =>
        ExecutionAnalysisError(error)
    }
  }
}

/**
 * Implements RequestListeners for use by ReadOnlyServerEngine, with methods
 *  to create transformed copies.
 */
private final case class TransformableRequestListeners(
  buildListeners: SbtClient = NullSbtClient,
  keyListeners: Seq[KeyValueClientListener[_]] = Seq.empty) extends RequestListeners {

  /** Remove a client from any registered listeners. */
  def disconnect(client: SbtClient): TransformableRequestListeners =
    copy(
      buildListeners = buildListeners without client,
      keyListeners = keyListeners map (_ remove client))

  def addBuildListener(l: SbtClient): TransformableRequestListeners = {
    val next = buildListeners zip l
    copy(buildListeners = next)
  }
  def removeBuildListener(l: SbtClient): TransformableRequestListeners = {
    val next = buildListeners without l
    copy(buildListeners = next)
  }

  def addKeyListener[T](client: SbtClient, key: sbt.ScopedKey[T]): TransformableRequestListeners = {
    // TODO - Speed this up.
    val handler =
      keyListeners.find(_.key == key).getOrElse(KeyValueClientListener(key, NullSbtClient))
    val newListeners = keyListeners.filterNot(_.key == key) :+ handler.add(client)
    copy(keyListeners = newListeners)
  }
  def removeKeyListener[T](client: SbtClient, key: sbt.ScopedKey[T]): TransformableRequestListeners = {
    keyListeners.find(_.key == key) map { handler =>
      val withoutHandler = keyListeners.filterNot(_.key == key)
      val newHandler = handler.remove(client)
      val newListeners = if (newHandler.client != NullSbtClient)
        withoutHandler :+ newHandler
      else
        withoutHandler
      copy(keyListeners = newListeners)
    } getOrElse {
      this
    }
  }
}
