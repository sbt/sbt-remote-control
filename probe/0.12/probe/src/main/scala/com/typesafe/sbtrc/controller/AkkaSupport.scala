package com.typesafe.sbtrc
package controller

import sbt._
import SbtUtil.extract

object AkkaSupport {
  def isAkkaProject(state: State, ref: Option[ProjectRef] = None): Boolean = {
    val key =
      ref match {
        case Some(project) => Keys.dependencyClasspath in Compile in project
        case _ => Keys.dependencyClasspath in Compile
      }
    val (_, classpath: Seq[Attributed[File]]) = extract(state).runTask(key, state)
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