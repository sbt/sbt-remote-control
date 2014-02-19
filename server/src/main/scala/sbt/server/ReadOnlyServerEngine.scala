package sbt
package server

import java.util.Queue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean
import sbt.protocol._

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
  @volatile
  private var serverState = ServerState()
  private val running = new AtomicBoolean(true)
  /** TODO - we should use data structure that allows us to merge/purge requests. */
  private val commandQueue: BlockingQueue[ServerRequest] =
    new java.util.concurrent.ArrayBlockingQueue[ServerRequest](10) // TODO - this should limit the number of queued requests for now
  // TODO - We should probably limit the number of deferred client requests so we don't explode during startup to DoS attacks...
  private val deferredStartupBuffer = collection.mutable.ArrayBuffer.empty[ServerRequest]

  private def updateState(f: ServerState => ServerState): Unit =
    serverState = f(serverState)

  override def run() {
    while (running.get && nextStateRef.get == null) {
      val ServerRequest(client, serial, request) = queue.take
      handleRequestsNoBuildState(client, serial, request)
    }
    // Now we flush through all the events we had.
    if (running.get) {
      // Notify all listener's we're ready.
      serverState.eventListeners.send(NowListeningEvent)
      for {
        ServerRequest(client, serial, request) <- deferredStartupBuffer
      } handleRequestsWithBuildState(client, serial, request, nextStateRef.get)
    }
    // Now we just run with the initialized build.
    while (running.get) {
      val ServerRequest(client, serial, request) = queue.take
      handleRequestsWithBuildState(client, serial, request, nextStateRef.get)
    }
  }
  /** Object we use to synch requests/state between event loop + command loop. */
  final object engineQueue extends ServerEngineQueue {
    // TODO - We just want volatile read of serverState, we don't need the whole variable to be
    // volatile always...
    // But the blocking command queue makes us not care about that performance hit at all.
    def takeNextRequest: (ServerState, ServerRequest) = (serverState, commandQueue.take)
  }

  private def handleRequestsNoBuildState(client: LiveClient, serial: Long, request: Request): Unit =
    request match {
      case ListenToEvents() =>
        // We do not send a listening message, because we aren't yet.
        updateState(_.addEventListener(client))
      case ClientClosedRequest() =>
        updateState(_.disconnect(client))
      case req: ExecutionRequest =>
        commandQueue.add(ServerRequest(client, serial, request))
      case _ =>
        // Defer all other messages....
        deferredStartupBuffer.append(ServerRequest(client, serial, request))
    }
  private def handleRequestsWithBuildState(client: LiveClient, serial: Long, request: Request, buildState: State): Unit =
    request match {
      case ListenToEvents() =>
        client.send(NowListeningEvent)
        updateState(_.addEventListener(client))
      case ListenToBuildChange() =>
        BuildStructureCache.sendBuildStructure(client, SbtDiscovery.buildStructure(buildState))
        updateState(_.addBuildListener(client))
      case ClientClosedRequest() =>
        updateState(_.disconnect(client))
      case ListenToValue(key) =>
        // TODO - We also need to get the value if it's a setting
        // and send it immediately...
        SbtToProtocolUtils.protocolToScopedKey(key, buildState) match {
          case Some(key) =>
            // Schedule the key to run as well as registering the key listener.
            val extracted = Project.extract(buildState)
            updateState(_.addKeyListener(client, key))
            commandQueue.add(ServerRequest(client, serial, ExecutionRequest(extracted.showKey(key))))
          case None => // Issue a no such key error
            client.reply(serial, ErrorResponse(s"Unable to find key: $key"))
        }
      case req: ExecutionRequest =>
        // TODO - Handle "queue is full" issues.
        commandQueue.add(ServerRequest(client, serial, request))
      case CommandCompletionsRequest(id, line, level) =>
        val combined = buildState.combinedParser
        val completions = complete.Parser.completions(combined, line, level)
        def convertCompletion(c: complete.Completion): protocol.Completion =
          protocol.Completion(
            c.append,
            c.display,
            c.isEmpty)
        client.send(CommandCompletionsResponse(id, completions.get map convertCompletion))
    }

}