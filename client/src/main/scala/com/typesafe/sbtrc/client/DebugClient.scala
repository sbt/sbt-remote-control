package com.typesafe.sbtrc
package client

import java.net._
import api._

object DebugClient {
  def apply(port: Int): SbtClient = {
    val client = new ipc.Client(new Socket("127.0.0.1", port))
    new SimpleSbtClient(client, closeHandler = () => ())
  }
}
