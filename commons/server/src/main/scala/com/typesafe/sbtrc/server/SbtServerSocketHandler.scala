package com.typesafe.sbtrc
package server

import ipc.{Server=>IpcServer, JsonWriter}
import com.typesafe.sbtrc.protocol.{Envelope, Request}
import ipc.{Server => IpcServer}
import java.net.ServerSocket

/**
 * A class that will spawn up a thread to handle client connection requests.
 * 
 * TODO - We should use netty for this rather than spawning so many threads.
 */
class SbtServerSocketHandler(serverSocket: ServerSocket, msgHandler: ClientRequest => Unit) {
  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  private val thread = new Thread {
    
    val clients = collection.mutable.ArrayBuffer.empty[SbtClientHandler]
    final override def run(): Unit = {
      while(running.get) {
        val nextConnection = new IpcServer(serverSocket)
        val id = java.util.UUID.randomUUID.toString
        val client = new SbtClientHandler(id, nextConnection, msgHandler)
        println("Client connected: " + id)
        clients.append(client)
      }
      // Cleanup clients, waiting for them to notify their users.
      clients.foreach(_.shutdown())
      clients.foreach(_.join())
    }
  }
  thread.start()
  
  // Tells the server to stop running.
  def stop(): Unit = running.set(false)
  
  // Blocks the server until we've been told to shutdown by someone.
  def join(): Unit = thread.join()
}