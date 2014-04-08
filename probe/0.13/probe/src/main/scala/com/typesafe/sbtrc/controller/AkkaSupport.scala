package com.typesafe.sbtrc
package controller

import sbt._

object AkkaSupport {
  def akkaVersion(state: State): Option[String] = {
    PoorManDebug.trace("Checking for Akka version.")
    val (_, classpath: Seq[Attributed[File]]) = Project.extract(state).runTask(Keys.dependencyClasspath in Compile, state)
    val maybeAkkaVersions = classpath map { file =>
      for {
        id <- file.get(Keys.moduleID.key)
        if id.organization == "com.typesafe.akka"
        if id.name contains "akka"
      } yield id.revision
    }
    maybeAkkaVersions.filter(_.isDefined).headOption.getOrElse(None)
  }

  def isAkkaProject(state: State): Boolean = {
    PoorManDebug.trace("Checking to see if this is an akka project.")
    akkaVersion(state).isDefined
  }
}