package sbt
package server

import java.util.Queue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean
import sbt.protocol._

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
  // IPC protocol requests which turn into ServerEngineWork go in here
  private val workRequestsQueue: BlockingQueue[ServerRequest] =
    new java.util.concurrent.ArrayBlockingQueue[ServerRequest](10) // TODO - this should limit the number of queued requests for now
  // TODO - We should probably limit the number of deferred client requests so we don't explode during startup to DoS attacks...
  private val deferredStartupBuffer = collection.mutable.ArrayBuffer.empty[ServerRequest]

  // TODO - This only works because it is called from a single thread.
  private def updateState(f: ServerState => ServerState): Unit =
    serverStateRef.lazySet(f(serverStateRef.get))

  override def run() {
    while (running.get && nextStateRef.get == null) {
      // here we poll, on timeout we check to see if we have build state yet.
      // We give at least one second for loading the build before timing out.
      queue.poll(1, java.util.concurrent.TimeUnit.SECONDS) match {
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
      // loop is started.p
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

  /** Object we use to synch requests/state between event loop + command loop. */
  final object engineQueue extends ServerEngineQueue {
    override def pollNextRequest(): Either[ServerState, ServerRequest] = {
      Option(workRequestsQueue.poll()) match {
        case Some(req) => Right(req)
        case None => Left(serverState)
      }
    }

    override def takeNextRequest(): ServerRequest = {
      workRequestsQueue.take()
    }
  }

  private def syntheticExecuteRequest(client: LiveClient, serial: Long, scopedKey: sbt.ScopedKey[_], buildState: State): Unit = {
    handleRequestsWithBuildState(client, serial, ExecutionRequest(Def.showFullKey(scopedKey)), buildState)
  }
  private def handleRequestsNoBuildState(client: LiveClient, serial: Long, request: Request): Unit =
    request match {
      case ListenToEvents() =>
        // We do not send a listening message, because we aren't yet.
        updateState(_.addEventListener(client))
        client.reply(serial, ReceivedResponse())
      case ClientClosedRequest() =>
        updateState(_.disconnect(client))
        client.reply(serial, ReceivedResponse())
      case _ =>
        // Defer all other messages....
        deferredStartupBuffer.append(ServerRequest(client, serial, request))
    }
  private def handleRequestsWithBuildState(client: LiveClient, serial: Long, request: Request, buildState: State): Unit =
    request match {
      case ListenToEvents() =>
        updateState(_.addEventListener(client))
        client.reply(serial, ReceivedResponse())
      case ListenToBuildChange() =>
        updateState(_.addBuildListener(client))
        BuildStructureCache.sendBuildStructure(client, SbtDiscovery.buildStructure(buildState))
        client.reply(serial, ReceivedResponse())
      case ClientClosedRequest() =>
        updateState(_.disconnect(client))
        client.reply(serial, ReceivedResponse())
      case KeyLookupRequest(key) =>
        val parser: complete.Parser[Seq[sbt.ScopedKey[_]]] = Act.aggregatedKeyParser(buildState)
        import SbtToProtocolUtils.scopedKeyToProtocol
        complete.Parser.parse(key, parser) match {
          case Right(sk) => client.reply(serial, KeyLookupResponse(key, sk.map(k => scopedKeyToProtocol(k))))
          case Left(msg) => client.reply(serial, KeyLookupResponse(key, Seq.empty))
        }
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
              // we set serial=0 because we don't want to generate a reply for the
              // synthetic ExecutionRequest generated in this call
              syntheticExecuteRequest(client, serial = 0L, scopedKey, buildState)
            }
            client.reply(serial, ReceivedResponse())

          case None => // Issue a no such key error
            client.reply(serial, KeyNotFound(key))
        }
      case req: ExecutionRequest =>
        // TODO - Handle "queue is full" issues.
        workRequestsQueue.add(ServerRequest(client, serial, request))
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

}
