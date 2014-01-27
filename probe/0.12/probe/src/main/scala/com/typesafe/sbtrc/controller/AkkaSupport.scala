package com.typesafe.sbtrc
package controller

import sbt._
import SbtUtil.extract

object AkkaSupport {
  def isAkkaProject(state: State): Boolean = {
    val (_, classpath: Seq[Attributed[File]]) = extract(state).runTask(Keys.dependencyClasspath in Compile, state)
    classpath exists { file =>
      val hasAkka =
        for {
          id <- file.get(Keys.moduleID.key)
          if id.organization == "com.typesafe.akka"
          if id.name contains "akka"
        } yield true
      hasAkka getOrElse false
    }
  }

  def validAkkaVersion(state: State, maxVersion: String): Boolean = {
    val (_, classpath: Seq[Attributed[File]]) = extract(state).runTask(Keys.dependencyClasspath in Compile, state)
    classpath exists { f =>
      val validAkka =
        for {
          id <- f.get(Keys.moduleID.key)
          if id.organization == "com.typesafe.akka"
          if id.name contains "akka"
          if AtmosSupport.convertVersionString(id.revision) <= AtmosSupport.convertVersionString(maxVersion)
        } yield true
      validAkka getOrElse false
    }
  }
}