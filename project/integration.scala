import sbt._
import Keys._
import SbtSupport.sbtLaunchJar
import xsbt.api.Discovery
import IvyRepositories.{localRepoCreated, makeLocalRepoSettings}

case class IntegrationTestResult(name: String, passed: Boolean, log: File)

/** This helps set up tests which run inside the sbt launcher. */
object integration {

  val mains = TaskKey[Seq[String]]("integration-test-mains", "Discovered integration test main classes")
  val itContext = TaskKey[IntegrationContext]("integration-test-context")
  val tests = TaskKey[Seq[IntegrationTestResult]]("integration-tests", "Runs all integration tests")
  val singleTest = InputKey[Seq[IntegrationTestResult]]("integration-test-only", "Runs integration tests that match the given glob")

  val knownBadClasses = Set("com.typesafe.sbtrc.TestUtil")

  def settings(publishedProjects: Seq[Project], itProject: Project): Seq[Setting[_]] =
    makeLocalRepoSettings(publishedProjects) ++ Seq(
    // Make sure we publish this project.
    mains <<= compile in Compile in itProject map { a =>
      val defs = a.apis.internal.values.flatMap(_.api.definitions)
      val results = Discovery(Set(), Set())(defs.toSeq)
      results collect { 
        case (df, di) if !di.isModule && !df.modifiers.isAbstract && !knownBadClasses.contains(df.name) => df.name
      } filter { name =>
        (name contains "Test")  || (name contains "Can")
      }
    },
    itContext <<= (sbtLaunchJar, localRepoCreated, streams, version, target, scalaVersion) map IntegrationContext.apply,
    // TODO - A much smarter integration test launcher!
    tests <<= (itContext, mains, streams) map { (ctx, ms, s) =>
      val results = ms map ctx.runTest
      handleResults(results, s)
      results
    },	
    singleTest <<= inputTask { argTask =>
      (argTask, itContext, mains, streams) map { (args, ctx, mains, s ) =>
        val glob = args mkString " "
        val results = mains filter (_ contains glob) map ctx.runTest
        handleResults(results, s)
        results
      }
    }
  )

  def handleResults(results: Seq[IntegrationTestResult], out: TaskStreams): Unit = {
    // TODO - Only colorize if we're in ANSI terminal.
    out.log.info(scala.Console.BLUE + " --- Integration Test Report ---" + scala.Console.BLUE_B)
    val maxName = results.map(_.name.length).max
    def padName(name: String): String = {
      val pad = Stream.continually(' ').take(maxName - name.length).mkString("")
      pad + name
    }
    for(result <- results.sortBy(r => r.passed + r.name)) {
      val resultString =
        if(result.passed) "[ " + scala.Console.GREEN + "PASSED"+ scala.Console.RESET +" ]"
        else              "[ " + scala.Console.RED + "FAILED"+ scala.Console.RESET +" ]"
      val seeString =
        if(result.passed) ""
        else (" see " + result.log.getAbsolutePath+scala.Console.RESET)
      out.log.info(" * " + padName(result.name) + " " + resultString + seeString)
    }
    if(results.exists(!_.passed)) {
      sys.error("Failing integration tests!")
    }
  }
}



// TODO - Time how long tests take.
// TODO - Send all logs to file, and maybe just filter errors up?
case class IntegrationContext(launchJar: File, 
                               repository: File,
                               streams: TaskStreams,
                               version: String,
                               target: File,
                               scalaVersion: String) {
  def runTest(name: String): IntegrationTestResult = {
    streams.log.info(scala.Console.BLUE+" [IT] Running: " + name + " [IT]"+scala.Console.BLUE_B)
    val friendlyName = name replaceAll("\\.", "-")
    val cwd = target / "integration-test" / friendlyName
    val logFile = target / "integration-test" / (friendlyName + ".log")
    sbt.IO.touch(logFile)
    val fileLogger = ConsoleLogger(new java.io.PrintWriter(new java.io.FileWriter(logFile)))
    // TODO - Filter logs so we're not so chatty on the console.
    val logger = new MultiLogger(List(fileLogger, streams.log.asInstanceOf[AbstractLogger]))
    // First clean the old test....
    IO delete cwd
    IO createDirectory cwd
    // Here, let's create a new logger that can store logs in a location of our choosing too...
    setup(name, cwd) ! logger match {
      case 0 => 
        streams.log.info(" [IT] " + name + " result: SUCCESS")
        IntegrationTestResult(name, true, logFile)
      case n => 
        streams.log.error(" [IT] " + name + " result: FAILURE")
        IntegrationTestResult(name, false, logFile)
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
       |  it-local: %s, [organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]
       |  local
       |  typesafe-ivy-releases: http://repo.typesafe.com/typesafe/ivy-releases/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
       |  typesafe-ivy-snapshots: http://repo.typesafe.com/typesafe/ivy-snapshots/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
       |  maven-central
       |
       |[boot]
       |  directory: ${sbt.boot.directory}
       |
       |[ivy]
       |  ivy-home: %s/.ivy2
       |  checksums: ${sbt.checksums-sha1,md5}
       |  override-build-repos: ${sbt.override.build.repos-false}
       |""".stripMargin format (scalaVersion, version, name, repository.toURI.toString, cwd.getAbsolutePath, cwd.getAbsolutePath)
}
