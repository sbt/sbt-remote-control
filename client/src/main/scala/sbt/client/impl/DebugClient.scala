
package sbt.client.impl

import sbt.impl.ipc
import java.net._
import sbt.client._
import sbt.protocol._

private[sbt] object DebugClient {

  def apply(port: Int): SbtClient = {
    val client = new ipc.Client(new Socket("127.0.0.1", port))
    val uuid = java.util.UUID.randomUUID()
    val configName = "debug-client"
    val humanReadableName = "Debug Client"
    client.sendJson[Message](RegisterClientRequest(ClientInfo(uuid.toString, configName, humanReadableName, ProtocolVersion1, Vector.empty)),
      client.serialGetAndIncrement())
    val channel = new SimpleSbtChannel(uuid, configName, humanReadableName, client, closeHandler = () => ())
    SbtClient(channel)
  }
}
