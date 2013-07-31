/*
 * Copyright 2012 Typesafe, Inc.
 * Based on sbt IPC code copyright 2009 Mark Harrah
 */

package com.typesafe.sbtrc

import java.net.{ InetAddress, ServerSocket, Socket }
import java.io.IOException
import java.nio.charset.Charset

package object ipc {
  private[ipc] val loopback = InetAddress.getByName(null)

  private[ipc] def ignoringIOException[T](block: => T): Unit = {
    try {
      block
    } catch {
      case e: IOException => ()
    }
  }

  private[ipc] val version = "1"
  private[ipc] val ServerGreeting = "I am Server: " + version
  private[ipc] val ClientGreeting = "I am Client: " + version

  private[ipc] val utf8 = Charset.forName("UTF-8")

  def openServerSocket(): ServerSocket = {
    new ServerSocket(0, 1, loopback)
  }

  def accept(serverSocket: ServerSocket): Server = {
    new Server(serverSocket)
  }

  def openClient(port: Int): Client = {
    new Client(new Socket(loopback, port))
  }
}
