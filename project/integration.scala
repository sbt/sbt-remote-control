import sbt._
import Keys._
import SbtSupport.sbtLaunchJar
import xsbt.api.Discovery
import IvyRepositories.{localRepoCreated, makeLocalRepoSettings}


/** This helps set up tests which run inside the sbt launcher. */
object integration {
  
  val mains = TaskKey[Seq[String]]("integration-test-mains", "Discovered integration test main classes")
  val itContext = TaskKey[IntegrationContext]("integration-test-context")
  val tests = TaskKey[Unit]("integration-tests", "Runs all integration tests")
  val singleTest = InputKey[Unit]("integration-test-only", "Runs integration tests that match the given glob")
  
  def settings(publishedProjects: Seq[Project], itProject: Project): Seq[Setting[_]] =
    makeLocalRepoSettings(publishedProjects) ++ Seq(
    // Make sure we publish this project.
    mains <<= compile in Compile in itProject map { a =>
      val defs = a.apis.internal.values.flatMap(_.api.definitions)
      val results = Discovery(Set("xsbti.Main"), Set())(defs.toSeq)
      results collect { 
        case (df, di) if !di.isModule && !df.modifiers.isAbstract => df.name
      }
    },
    itContext <<= (sbtLaunchJar, localRepoCreated, streams, version, target, scalaVersion) map IntegrationContext.apply,
    tests <<= (itContext, mains) map { (ctx, ms) =>
      ms foreach ctx.runTest
    },	
    singleTest <<= inputTask { argTask =>
      (argTask, itContext, mains) map { (args, ctx, mains) =>
        val glob = args mkString " "
        mains filter (_ contains glob) foreach ctx.runTest
      }
    }
  )
}




case class IntegrationContext(launchJar: File, 
                               repository: File,
                               streams: TaskStreams,
                               version: String,
                               target: File,
                               scalaVersion: String) {
  def runTest(name: String): Unit = {
    streams.log.info(" [IT] Running: " + name + " [IT]")
    val friendlyName = name replaceAll("\\.", "-")
    val cwd = target / "integration-test" / friendlyName
    val logFile = target / "integration-test" / (friendlyName + ".log")
    sbt.IO.touch(logFile)
    val fileLogger = ConsoleLogger(new java.io.PrintWriter(new java.io.FileWriter(logFile)))
    val logger = new MultiLogger(List(fileLogger, streams.log.asInstanceOf[AbstractLogger]))
    // First clean the old test....
    IO delete cwd
    IO createDirectory cwd
    // Here, let's create a new logger that can store logs in a location of our choosing too...
    setup(name, cwd) ! logger match {
      case 0 => 
        streams.log.info( " [IT] " + name + " result: SUCCESS [IT]")
      case n => 
        streams.log.error(" [IT] " + name + " result: FAILURE [IT]")
        sys.error("Integration test failed")
    }
  }
  
  
  
  private def setup(name: String, cwd: File): ProcessBuilder = {
    val props = cwd / "sbt.boot.properties"
    IO.write(props, makePropertiesString(name, cwd))
    IO createDirectory (cwd / "project")
    // TODO - Is this needed?
    IO.write(cwd / "project" / "build.properties", "sbt.version=" +
        Dependencies.sbt12Version)
    val boot = cwd / "boot"
    Process(Seq("java", 
        "-Dsbt.boot.properties=" + props.getAbsolutePath, 
        "-Dsbt.boot.directory=" + boot.getAbsolutePath, 
        "-Dakka.loglevel=DEBUG",
            "-Dakka.actor.debug.autoreceive=on",
            "-Dakka.actor.debug.receive=on",
            "-Dakka.actor.debug.lifecycle=on",
        "-jar", 
        launchJar.getAbsolutePath), cwd)
  }
  
  // TODO - Pull information from the current project...
  private def makePropertiesString(name: String, cwd: File): String =
    """|[scala]
       |  version: %s
       |
       |[app]
       |  org: com.typesafe.sbtrc
       |  name: sbt-rc-integration-tests
       |  version: %s
       |  class: %s
       |  cross-versioned: false
       |  components: xsbti
       |
       |[repositories]
       |  it-local: file://%s, [organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]
       |
       |[boot]
       |  directory: ${sbt.boot.directory}
       |
       |[ivy]
       |  ivy-home: %s/.ivy2
       |  checksums: ${sbt.checksums-sha1,md5}
       |  override-build-repos: ${sbt.override.build.repos-false}
       |""".stripMargin format (scalaVersion, version, name, repository, cwd.getAbsolutePath, cwd.getAbsolutePath)
}
