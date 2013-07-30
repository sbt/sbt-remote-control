package com.typesafe.sbtrc

import sbt._

/** Implicit hacks to smooth version issues. */
object SbtCustomHacks {

  //type ScopedKey[T] = sbt.Def.ScopedKey[T]
  //type Setting[T] = sbt.Def.Setting[T]

  /** Can convert an event logger into GlobalLogging. */
  implicit class globalLoggingChanges(l: EventLogger) {
    private def throwawayBackingFile = java.io.File.createTempFile("activator-", ".log")
    private def newBacking =
      GlobalLogBacking(file = throwawayBackingFile,
        last = None,
        newBackingFile = () => throwawayBackingFile)
    def toGlobalLogging: GlobalLogging =
      GlobalLogging(
        full = l,
        console = l.consoleOut,
        // TODO - is this correct?
        backed = ConsoleLogger(l.consoleOut),
        backing = newBacking,
        newLogger = (writer, oldBacking) => toGlobalLogging)
  }

  /** Returns our "Show[ScopedKey[_]]" */
  def showFullKey(state: State) = Project.showContextKey(state)

  // TODO - is this correct?
  def dontShowAggregate: sbt.Aggregation.ShowConfig =
    Aggregation.ShowConfig(settingValues = false, taskValues = false, print = println _, success = false)
}