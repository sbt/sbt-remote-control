package com.typesafe.sbtrc
package client

import java.net._
import sbt.client._
import sbt.protocol.RegisterClientRequest
import sbt.protocol.ClientInfo
import sbt.protocol.registerClientRequestFormat

object DebugClient {
  def apply(port: Int): SbtClient = {
    val client = new ipc.Client(new Socket("127.0.0.1", port))
    val uuid = java.util.UUID.randomUUID()
    val configName = "debug-client"
    val humanReadableName = "Debug Client"
    client.sendJson(RegisterClientRequest(ClientInfo(uuid.toString, configName, humanReadableName)),
      client.serialGetAndIncrement())
    val channel = new SimpleSbtChannel(uuid, configName, humanReadableName, client, closeHandler = () => ())
    SbtClient(channel)
  }
}
