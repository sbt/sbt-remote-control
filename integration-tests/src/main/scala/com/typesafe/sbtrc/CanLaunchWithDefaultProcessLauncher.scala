package com.typesafe.sbtrc

import com.typesafe.sbtrc.it._
import java.io.File
import akka.actor._
import akka.pattern._
import akka.dispatch._
import concurrent.duration._
import concurrent.Await
import akka.util.Timeout
import xsbti.Repository
import xsbti.MavenRepository
import xsbti.IvyRepository

trait SbtLauncherTest extends IntegrationTest {
  /** The local repositories we need to use for this integration test. */
  def localRepositories: Seq[(String, File, Option[String])] = {
    def getFileLocationAndName(repo: Repository): Option[(String, File, Option[String])] =
      repo match {
        // TODO - Support maven too?
        case x: IvyRepository if x.url.getProtocol == "file" =>
          Some(RepoHelper.ivy(x.id, new File(x.url.toURI)))
        case x: MavenRepository if x.url.getProtocol == "file" =>
          Some(RepoHelper.mvn(x.id, new File(x.url.toURI)))
        case _ => None
      }

    for {
      repo <- repositories
      values <- getFileLocationAndName(repo)
    } yield values
  }
  /** Constructs a new sbt process launcher using the repositories from our own launched app. */
  def sbtProcessLauncher: SbtProcessLauncher =
    new DefaultSbtProcessLauncher(configuration,
      // TODO - Figure out a way to prevent reading the user's ~/.sbt/repositories file in favor
      // of our locally defined repositories....
      optionalRepositories = localRepositories)

}

class CanLaunchThroughSbtLauncher extends SbtLauncherTest {
  val system = ActorSystem("ManualTest")

  try {
    // TODO - Create project here, rather than rely on it created by test harness....
    val dir = new File("dummy")
    makeDummySbtProject(dir)
    val child = SbtProcess(system, dir, sbtProcessLauncher)
    try {
      implicit val timeout = Timeout(300.seconds)
      val name = Await.result(child ? protocol.NameRequest(sendEvents = false), timeout.duration) match {
        case protocol.NameResponse(n) => {
          n
        }
        case protocol.ErrorResponse(error) =>
          throw new Exception("Failed to get project name: " + error)
      }
      println("Project is: " + name)
      val compiled = Await.result(child ? protocol.CompileRequest(sendEvents = false), timeout.duration) match {
        case protocol.CompileResponse(success) => {
          success
        }
        case protocol.ErrorResponse(error) =>
          throw new Exception("Failed to compile: " + error)
      }
      println("compiled=" + compiled)
      val run = Await.result(child ? protocol.RunRequest(sendEvents = false, mainClass = None), timeout.duration) match {
        case protocol.RunResponse(success, "run") => {
          success
        }
        case protocol.ErrorResponse(error) =>
          throw new Exception("Failed to run: " + error)
      }
      println("run=" + run)
    } finally {
      system.stop(child)
    }
  } finally {
    system.shutdown()
    system.awaitTermination()
  }
}
