package com.typesafe.sbtrc

import properties.SbtRcProperties._
import sbt.IO
// Helper methods for running tests.
package object it {

  // This method has to be used around any code the makes use of Akka to ensure the classloader is right.
  def withContextClassloader[A](f: => A): A = {
    val current = Thread.currentThread
    val old = current.getContextClassLoader
    current setContextClassLoader getClass.getClassLoader
    try f
    finally current setContextClassLoader old
  }

  // Success and failure conditions for tests.
  case object Success extends xsbti.Exit {
    val code = 0
  }
  case object Failure extends xsbti.Exit {
    val code = 1
  }

  def createFile(name: java.io.File, content: String): Unit = {
    val writer = new java.io.FileWriter(name)
    try writer.write(content)
    finally writer.close()
  }

  /** Creates a dummy project we can run Activator against. */
  def makeDummySbtProject(dir: java.io.File): java.io.File = {
    IO.createDirectory(dir)
    val project = new java.io.File(dir, "project")
    IO.createDirectory(project)
    val props = new java.io.File(project, "build.properties")
    createFile(props, "sbt.version=" + SBT_VERSION)
    val scalaSource = new java.io.File(dir, "src/main/scala")
    IO.createDirectory(scalaSource)
    val main = new java.io.File(scalaSource, "hello.scala")
    createFile(main, "object Main extends App { println(\"Hello World\") }\n")
    dir
  }

  /** Creates a dummy project we can run Activator against. */
  def makeDummyPlayProject(dir: java.io.File): java.io.File = {
    makeDummySbtProject(dir)
    val project = new java.io.File(dir, "project")
    val playPlugin = new java.io.File(project, "play.sbt")
    createFile(playPlugin, s"""addSbtPlugin("play" % "sbt-plugin" % "2.1.0")\n""")
    val playBuild = new java.io.File(project, "build.scala")
    createFile(playBuild, s"""|import sbt._
                              |object MyBuild extends Build {
                              |  val root = play.Project("default-play")
                              |}""".stripMargin)
    dir
  }
}