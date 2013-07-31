package com.typesafe

package object sbtrc {

  private[sbtrc] def loggingFailure[T](log: akka.event.LoggingAdapter)(block: => T): T = try {
    block
  } catch {
    case e: Exception =>
      log.error(e, e.getMessage)
      throw e
  }

}
