package com.typesafe.sbtrc
package server

import java.net.ServerSocket
import sbt.State
import sbt.server.ServerEngine
import sbt.server.ServerRequest
import sbt.protocol

/**
 * This class implements the core sbt engine.   We delegate all behavior down to a single
 * threaded sbt execution engine.
 */
class SbtServer(configuration: xsbti.AppConfiguration, socket: ServerSocket) extends xsbti.Server {

  override val uri: java.net.URI = {
    val port = socket.getLocalPort
    val addr = socket.getInetAddress.getHostAddress
    new java.net.URI(s"http://${addr}:${port}")
  }
  // The queue where requests go before we fullfill them.
  private val queue = new java.util.concurrent.LinkedBlockingDeque[ServerRequest]
  // External API to run queue.
  def queueClientRequest(request: ServerRequest): Unit = queue.add(request)
  // Create the helper which will handle socket requests.
  private val socketHandler = new SbtServerSocketHandler(socket, queueClientRequest)

  private val stateRef = new java.util.concurrent.atomic.AtomicReference[State](null)
  private val eventEngine = new sbt.server.ReadOnlyServerEngine(queue, stateRef)
  private val commandEngine = new sbt.server.ServerEngine(eventEngine.engineWorkQueue, stateRef)
  // TODO - Maybe the command engine should extend thread too?
  private val commandEngineThread = new Thread("sbt-server-command-loop") {
    override def run(): Unit = {
      val originOut = System.out
      val originErr = System.err
      // TODO - Timeouts that lead to us shutting down the server.
      try commandEngine.execute(configuration)
      catch {
        case e: Throwable =>
          e.printStackTrace(originErr)
          throw e
      }
      originOut.println("Done executing sbt server engine.")
      socketHandler.stop()
      socketHandler.join()
    }
  }
  override def awaitTermination(): xsbti.MainResult = {
    // Here we actually start.
    eventEngine.start()
    commandEngineThread.start()
    // Wait for the server to stop, then exit.
    commandEngineThread.join()
    // TODO - We should allow the server to tell us to reboot.
    Exit(0)
  }
}