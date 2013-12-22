package com.typesafe.sbtrc
package server

import ipc.{Server=>IpcServer, JsonWriter}
import com.typesafe.sbtrc.protocol.{Envelope, Request}
import ipc.{Server => IpcServer}
import java.net.ServerSocket
import java.net.SocketTimeoutException

/**
 * A class that will spawn up a thread to handle client connection requests.
 * 
 * TODO - We should use netty for this rather than spawning so many threads.
 */
class SbtServerSocketHandler(serverSocket: ServerSocket, msgHandler: ClientRequest => Unit) {
  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  private val TIMEOUT_TO_DEATH: Int = 3*60*1000
  private val thread = new Thread {
    val clients = collection.mutable.ArrayBuffer.empty[SbtClientHandler]
    // TODO - Check how long we've been running without a client connected
    // and shut down the server if it's been too long.
    final override def run(): Unit = {
      // TODO - Is this the right place to do this?
      serverSocket.setSoTimeout(TIMEOUT_TO_DEATH)
      while(running.get) {
        try {
          val nextConnection = new IpcServer(serverSocket)
          val id = java.util.UUID.randomUUID.toString
          def onClose(): Unit = {
            clients.remove(clients.indexWhere(_.id == id))
            // TODO - See if we can reboot the timeout on the server socket waiting
            // for another connection.
          }
          val client = new SbtClientHandler(id, nextConnection, msgHandler, onClose)
          clients.append(client)
        } catch {
          case e: java.io.EOFException =>
            // For now we ignore these, as someone trying to discover if we're listening to ports
            // will connect and close before we can read the handshake.
            // We should formalize what constitutes a catastrophic failure, and make sure we
            // down the server in that instance.
          case _: InterruptedException | _: SocketTimeoutException =>
            System.out.println("Checking to see if clients are empty...")
            // Here we need to check to see if we should shut ourselves down.
            if(clients.isEmpty) {
              System.out.println("No clients connected after 3 min.  Shutting down.")
              running.set(false)
            } else {
              System.out.println("We have a client, continuing serving connections.")
            }
          case e: Throwable =>
            // On any other failure, we'll just down the server for now.
            running.set(false)
        }
      }
      // Cleanup clients, waiting for them to notify their users.
      clients.foreach(_.shutdown())
      clients.foreach(_.join())
      // TODO - better shutdown semantics?
      System.exit(0)
    }
  }
  thread.start()
  
  // Tells the server to stop running.
  def stop(): Unit = running.set(false)
  
  // Blocks the server until we've been told to shutdown by someone.
  def join(): Unit = thread.join()
}