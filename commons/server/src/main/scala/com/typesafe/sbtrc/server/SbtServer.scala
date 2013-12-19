package com.typesafe.sbtrc
package server


import java.net.ServerSocket
import sbt.State


/**
 * This class implements the core sbt engine.   We delegate all behavior down to a single
 * threaded sbt execution engine.
 */
class SbtServer(engine: SbtServerEngine, socket: ServerSocket) {
  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  // The queue where requests go before we fullfill them.
  private val queue = new java.util.concurrent.LinkedBlockingDeque[ClientRequest]
  // External API to run queue.
  def queueClientRequest(request: ClientRequest): Unit = queue.add(request)
  // Create the helper which will handle socket requests.
  private val socketHandler = new SbtServerSocketHandler(socket, queueClientRequest)
  
  // TODO - Construct our engine, and then start handling events on some thread.
  private val thread = new Thread {
    override def run(): Unit = {
      // TODO - Timeouts that lead to us shutting down the server.
      while(engine.isRunning) {
        // First we wait for a new message.  This will block the current thread
        // until we have a message.
        engine.bufferRequest(queue.take)
        // Now we buffer any other messages we may find.
        while(!queue.isEmpty) {
          engine.bufferRequest(queue.take)
        }
        // Now that we're done, fullfill all the requests that were queued.
        engine.runRequests()
      }
      socketHandler.stop()
      socketHandler.join()
    }
  }
  // TODO - just start automatically?
  def start(): Unit = thread.start()
  def join(): Unit = thread.join()
}