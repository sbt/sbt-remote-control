package com.typesafe.sbtrc

import sbt._

/** Implicit hacks to smooth version issues. */
object SbtCustomHacks {

  //type ScopedKey[T] = sbt.Def.ScopedKey[T]
  //type Setting[T] = sbt.Def.Setting[T]

  /** Returns our "Show[ScopedKey[_]]" */
  def showFullKey(state: State) = Project.showContextKey(state)

  // TODO - is this correct?
  def dontShowAggregate: sbt.Aggregation.ShowConfig =
    Aggregation.ShowConfig(settingValues = false, taskValues = false, print = println _, success = false)
}