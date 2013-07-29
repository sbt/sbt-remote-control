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
}