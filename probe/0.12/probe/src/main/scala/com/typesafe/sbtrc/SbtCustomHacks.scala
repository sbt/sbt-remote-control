package com.typesafe.sbtrc

import sbt._

/** Implicit hacks to smooth version issues. */
object SbtCustomHacks {

  /** Can convert an event logger into GlobalLogging. */
  trait GlobalLoggingConvertor { def toGlobalLogging: GlobalLogging }
  implicit def globalLoggingChanges(l: EventLogger): GlobalLoggingConvertor =
    new GlobalLoggingConvertor {
      private def throwawayBackingFile = java.io.File.createTempFile("activator-", ".log")
      private def newBacking =
        GlobalLogBacking(file = throwawayBackingFile,
          last = None,
          newLogger = (writer, oldBacking) => toGlobalLogging,
          newBackingFile = () => throwawayBackingFile)
      def toGlobalLogging: GlobalLogging =
        GlobalLogging(l, ConsoleLogger(l.consoleOut), newBacking)
    }

  /** The Show we use when loading sbt projects. */
  def showFullKey(state: State) = Project.showFullKey

  /** Configuration to disable showing values for aggregate tasks. */
  def dontShowAggregate: Boolean = false
}