package com.typesafe.sbtrc
package controller

import sbt._

object AkkaSupport {
  def isAkkaProject(state: State): Boolean = {
    val (_, classpath: Seq[Attributed[File]]) = Project.extract(state).runTask(Keys.dependencyClasspath in Compile, state)
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
}