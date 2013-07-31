package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.Params

final class ParamsHelper(params: Params) {
  import params._
  // TODO we don't really want to depend on the "protocol" module from the UI interface.
  def toMap: Map[String, Any] = {
    if (contentType != "application/json")
      Map.empty
    else
      com.typesafe.sbtrc.protocol.Message.paramsFromJsonString(value)
  }
}
object ParamsHelper {
  def fromMap(map: Map[String, Any]): Params = {
    Params("application/json", com.typesafe.sbtrc.protocol.Message.paramsToJsonString(map))
  }

  implicit def p2Helper(params: Params): ParamsHelper = new ParamsHelper(params)
}
