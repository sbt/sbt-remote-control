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

// a little wrapper around protocol.request to keep the client/serial with it
case class ServerRequest(client: LiveClient, serial: Long, request: protocol.Request)

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
  queue: BlockingQueue[ServerRequest],
  nextStateRef: AtomicReference[State]) extends Thread("read-only-sbt-event-loop") {
  // TODO - We should have a log somewhere to store events.
  private var serverStateRef = new AtomicReference[ServerState](ServerState())
  def serverState = serverStateRef.get()
  private val running = new AtomicBoolean(true)
  // TODO - We should probably limit the number of deferred client requests so we don't explode during startup to DoS attacks...
  private val deferredStartupBuffer = collection.mutable.ArrayBuffer.empty[ServerRequest]

  // TODO - This only works because it is called from a single thread.
  private def updateState(f: ServerState => ServerState): Unit =
    serverStateRef.lazySet(f(serverStateRef.get))

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
    private val bufferedLogs = new LinkedBlockingQueue[EventWithWrites[protocol.LogEvent]]()
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
        case event: LogEvent if event.taskId == 0L =>
          eventListeners match {
            case NullSbtClient =>
              bufferedLogs.add(EventWithWrites.withWrites(event.asInstanceOf[T with LogEvent]))
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
    override def blockAndTakeNext: (ServerState, ServerEngineWork) =
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
        val state = serverState
        eventSink.send(protocol.ExecutionStarting(work.id.id))
        (state, work)
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
                case hd :: tail if hd.id == work.id => work :: tail
                case hd :: tail => hd :: insertWork(tail)
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
            workQueue = workQueue filterNot (_.id.id == id)
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

  override def run() {
    // we buffer most requests until 1) we have a state and 2) the project loads successfully.

    while (running.get && Option(nextStateRef.get).map(!Project.isProjectLoaded(_)).getOrElse(true)) {
      // we have to poll here because we want to continue even without
      // a request, if we get a state. Keep the poll short to avoid a needless
      // lag on startup.
      queue.poll(100, java.util.concurrent.TimeUnit.MILLISECONDS) match {
        case null => () // Ignore.
        case ServerRequest(client, serial, request) =>
          handleRequestsNoBuildState(client, serial, request)
      }
    }

    // Now we flush through all the events we had.
    // TODO - handle failures 
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
      val ServerRequest(client, serial, request) = queue.take
      // TODO - handle failures 
      try handleRequestsWithBuildState(client, serial, request, nextStateRef.get)
      catch {
        case e: Exception =>
          // TODO - Fatal exceptions?
          client.reply(serial, protocol.ErrorResponse(e.getMessage))
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
  private def killServer(): Unit = {
    System.exit(0)
  }
  private def handleRequestsNoBuildState(client: LiveClient, serial: Long, request: Request): Unit =
    request match {

      //// If you change any of these, you probably also need to change
      //// handleRequestsWithBuildState below.

      case KillServerRequest() =>
        killServer()
      case ListenToEvents() =>
        listenToEvents(client, serial)
      case UnlistenToEvents() =>
        unlistenToEvents(client, serial)
      case ClientClosedRequest() =>
        updateState(_.disconnect(client))
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
        killServer()
      case ListenToEvents() =>
        listenToEvents(client, serial)
      case UnlistenToEvents() =>
        unlistenToEvents(client, serial)
      case ClientClosedRequest() =>
        updateState(_.disconnect(client))
        client.reply(serial, ReceivedResponse())

      //// Second, requests we only handle when we have state,
      //// or handle differently when we have state.

      case ListenToBuildChange() =>
        updateState(_.addBuildListener(client))
        client.reply(serial, ReceivedResponse())
      case UnlistenToBuildChange() =>
        updateState(_.removeBuildListener(client))
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
              val change = SbtToProtocolUtils.settingKeyToProtocolValue(settingKey, extracted)
              client.send(ValueChanged(key, change))

              // register the key listener.
              // TODO: needs support somewhere to send events when the value of setting keys are updated
              updateState(_.addKeyListener(client, scopedKey))
            } else {
              // Schedule the key to run as well as registering the key listener.
              updateState(_.addKeyListener(client, scopedKey))
            }
            client.reply(serial, ReceivedResponse())

          case None => // Issue a no such key error
            client.reply(serial, KeyNotFound(key))
        }
      case UnlistenToValue(key) =>
        SbtToProtocolUtils.protocolToScopedKey(key, buildState) match {
          case Some(scopedKey) =>
            updateState(_.removeKeyListener(client, scopedKey))
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
              val change = SbtToProtocolUtils.settingKeyToProtocolValue(settingKey, extracted)
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
