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
class SbtServer(configuration: xsbti.AppConfiguration, socket: ServerSocket) extends ServerEngine with xsbti.Server {

  override val uri: java.net.URI = {
    val port = socket.getLocalPort
    val addr = socket.getInetAddress.getHostAddress
    new java.net.URI(s"http://${addr}:${port}")
  }
  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  // The queue where requests go before we fullfill them.
  private val queue = new java.util.concurrent.LinkedBlockingDeque[ServerRequest]
  // External API to run queue.
  def queueClientRequest(request: ServerRequest): Unit = queue.add(request)
  // Create the helper which will handle socket requests.
  private val socketHandler = new SbtServerSocketHandler(socket, queueClientRequest)

  override final def takeNextRequest: ServerRequest = {
    // TODO - Flush a bunch of events and merge/drop
    queue.take
  }

  override final def takeAllEventListenerRequests: Seq[ServerRequest] = {
    import collection.JavaConverters._
    val buf = new java.util.ArrayList[ServerRequest]
    queue.drainTo(buf)

    val (listeners, other) =
      buf.asScala.partition {
        case ServerRequest(_, protocol.ListenToEvents()) => true
        case _ => false
      }
    // TODO - make sure this is done correctly
    other.reverse.foreach(queue.addFirst)
    listeners
  }

  // TODO - Construct our engine, and then start handling events on some thread.
  private val thread = new Thread("sbt-server-main") {
    override def run(): Unit = {
      val originOut = System.out
      val originErr = System.err
      // TODO - Timeouts that lead to us shutting down the server.
      try execute(configuration)
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
  // TODO - just start automatically?
  def start(): Unit = thread.start()
  override def awaitTermination(): xsbti.MainResult = {
    // Wait for the server to stop, then exit.
    thread.join()
    // TODO - We should allow the server to tell us to reboot.
    Exit(0)
  }
}