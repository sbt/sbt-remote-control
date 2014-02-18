package com.typesafe.sbtrc
package it

import sbt.client._
import com.typesafe.sbtrc.client.{
  SimpleConnector,
  SimpleLocator,
  LaunchedSbtServerLocator
}
import java.io.File
import java.net.URL
import xsbti.{
  AppConfiguration,
  PredefinedRepository,
  MavenRepository,
  IvyRepository
}
import java.nio.charset.Charset.defaultCharset

trait SbtClientTest extends IntegrationTest {
  // TODO - load from config
  def defaultTimeout = concurrent.duration.Duration(60, java.util.concurrent.TimeUnit.SECONDS)

  def testingLocator: LaunchedSbtServerLocator = new LaunchedSbtServerLocator {
    // TODO - Do we care if the version for this directory is different?
    def sbtProperties(directory: File): URL =
      rewrittenPropsUrl

    lazy val rewrittenPropsUrl: URL = {
      val tmp = java.io.File.createTempFile("sbt-server", "properties")
      val existing = getClass.getClassLoader.getResource("sbt-server.properties")
      val oldLines = sbt.IO.readLinesURL(existing, defaultCharset)
      val newLines = makeNewLaunchProperties(oldLines)
      sbt.IO.writeLines(tmp, newLines, defaultCharset, false)
      tmp.deleteOnExit()
      tmp.toURI.toURL
    }

    private def repositories: List[String] = {
      configuration.provider.scalaProvider.launcher.ivyRepositories.toList map {
        case ivy: IvyRepository =>
          // TODO - We only support converting things we care about here, not unviersally ok.
          val pattern = Option(ivy.ivyPattern).map(",".+).getOrElse("")
          val aPattern = Option(ivy.artifactPattern).map(",".+).getOrElse("")
          val mvnCompat = if (ivy.mavenCompatible) ", mavenCompatible" else ""
          ivy.id + ": " + ivy.url + pattern + aPattern + mvnCompat
        case mvn: MavenRepository => mvn.id + ": " + mvn.url
        case predef: PredefinedRepository => predef.id.toString
      }
    }

    private def makeNewLaunchProperties(old: List[String]): List[String] = {
      val header = old.takeWhile(line => !line.contains("[repositories]"))
      val tail = old.dropWhile(line => !line.contains("[boot]"))
      header ++ ("[repositories]" :: repositories) ++ List("") ++ tail
    }
  }

  /**
   * Allows running tests against sbt.  Will block until sbt server is loaded against
   * a given directory...
   */
  final def withSbt(projectDirectory: java.io.File)(f: SbtClient => Unit): Unit = {
    // TODO - Create a prop-file locator that uses our own repositories to
    // find the classes, so we use cached values...
    val connector = new SimpleConnector(projectDirectory, testingLocator)
    // TODO - Executor for this thread....
    object runOneThingExecutor extends concurrent.ExecutionContext {
      private var task = concurrent.promise[Runnable]
      def execute(runnable: Runnable): Unit = synchronized {
        task.success(runnable)
      }
      // TODO - Test failure...
      def reportFailure(t: Throwable): Unit = task.failure(t)

      def runWhenReady(): Unit =
        // TODO - This is the wait time for us to connect to an sbt client....
        concurrent.Await.result(task.future, defaultTimeout).run()
    }
    val newHandler: SbtClient => Unit = { client =>
      // TODO - better error reporting than everything.
      (client handleEvents {
        msg => System.out.println(msg)
      })(concurrent.ExecutionContext.global)
      f(client)
    }
    // TODO - We may want to connect to the sbt server and dump debugging information/logs.
    val subscription = (connector onConnect newHandler)(runOneThingExecutor)
    // Block current thread until we can run the test.
    try runOneThingExecutor.runWhenReady()
    finally connector.close()
  }

  lazy val utils = new TestUtil(new java.io.File("scratch"))
}