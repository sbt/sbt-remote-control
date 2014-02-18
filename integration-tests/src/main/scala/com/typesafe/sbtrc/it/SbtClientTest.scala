package com.typesafe.sbtrc
package it

import sbt.client._
import com.typesafe.sbtrc.client.{
  SimpleConnector,
  SimpleLocator
}

trait SbtClientTest extends IntegrationTest {
  // TODO - load from config
  def defaultTimeout = concurrent.duration.Duration(60, java.util.concurrent.TimeUnit.SECONDS)

  /**
   * Allows running tests against sbt.  Will block until sbt server is loaded against
   * a given directory...
   */
  final def withSbt(projectDirectory: java.io.File)(f: SbtClient => Unit): Unit = {
    // TODO - Createa  prop-file locator that uses our own repositories to
    // find the classes, so we use cached values...
    val connector = new SimpleConnector(projectDirectory, SimpleLocator)
    // TODO - Executor for this thread....
    implicit object runOneThingExecutor extends concurrent.ExecutionContext {
      private var task = concurrent.promise[Runnable]
      def execute(runnable: Runnable): Unit = synchronized {
        task.success(runnable)
      }
      // TODO - Test failure...
      def reportFailure(t: Throwable): Unit = task.failure(t)

      def runWhenReady(): Unit =
        concurrent.Await.result(task.future, concurrent.duration.Duration.Inf).run()
    }
    val subscription = connector onConnect f
    // Block current thread until we can run the test.
    try runOneThingExecutor.runWhenReady()
    finally connector.close()
  }

  lazy val utils = new TestUtil(new java.io.File("scratch"))
}