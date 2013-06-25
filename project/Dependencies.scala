import sbt._
import Keys._

object Dependencies {
  val sbtVersion = "0.12.4-RC3"
  val sbtPluginVersion = "0.12"
  val sbtPluginScalaVersion = "2.9.2"
  val scalaVersion = "2.10.1"
  val sbtSnapshotVersion = "0.13.0-Beta2"
  val playVersion = "2.1.1"
  val akkaVersion = "2.1.2"


  val sbtIo210             = "org.scala-sbt" % "io" % sbtSnapshotVersion
  val sbtLauncherInterface = "org.scala-sbt" % "launcher-interface" % sbtVersion
  val sbtMain              = "org.scala-sbt" % "main" % sbtVersion
  val sbtTheSbt            = "org.scala-sbt" % "sbt" % sbtVersion
  val sbtIo                = "org.scala-sbt" % "io" % sbtVersion
  val sbtLogging           = "org.scala-sbt" % "logging" % sbtVersion
  val sbtProcess           = "org.scala-sbt" % "process" % sbtVersion
  
  // TODO - Don't use a snapshot version for this...
  val sbtCompletion           = "org.scala-sbt" % "completion" % sbtSnapshotVersion
  
  val akkaActor            = "com.typesafe.akka" % "akka-actor_2.10" % akkaVersion
  val akkaSlf4j            = "com.typesafe.akka" % "akka-slf4j_2.10" % akkaVersion
  val akkaTestkit          = "com.typesafe.akka" % "akka-testkit_2.10" % akkaVersion
  
  val commonsIo            = "commons-io" % "commons-io" % "2.0.1"

  val mimeUtil             = "eu.medsea.mimeutil" % "mime-util" % "2.1.1"
  // need to manually set this to override an incompatible old version
  val slf4jLog4j           = "org.slf4j" % "slf4j-log4j12" % "1.6.6"

  val junitInterface       = "com.novocode" % "junit-interface" % "0.7"
  //val specs2               = "org.specs2" % "specs2_2.10" % "1.13"

  // SBT plugins we have to shim
  val playSbtPlugin        =  Defaults.sbtPluginExtra("play" % "sbt-plugin" % playVersion, sbtPluginVersion, sbtPluginScalaVersion)
  val eclipseSbtPlugin     =  Defaults.sbtPluginExtra("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0", sbtPluginVersion, sbtPluginScalaVersion)
  val ideaSbtPlugin        =  Defaults.sbtPluginExtra("com.github.mpeltonen" % "sbt-idea" % "1.3.0", sbtPluginVersion, sbtPluginScalaVersion)
  val pgpPlugin            =  Defaults.sbtPluginExtra("com.typesafe.sbt" % "sbt-pgp" % "0.8", sbtPluginVersion, sbtPluginScalaVersion)


  // Mini DSL
  // DSL for adding remote deps like local deps.
  implicit def p2remote(p: Project): RemoteDepHelper = new RemoteDepHelper(p)
  final class RemoteDepHelper(p: Project) {
    def dependsOnRemote(ms: ModuleID*): Project = p.settings(libraryDependencies ++= ms)
  }
  // DSL for adding source dependencies ot projects.
  def dependsOnSource(dir: String): Seq[Setting[_]] = {
    import Keys._
    Seq(unmanagedSourceDirectories in Compile <<= (unmanagedSourceDirectories in Compile, baseDirectory) { (srcDirs, base) => (base / dir / "src/main/scala") +: srcDirs },
        unmanagedSourceDirectories in Test <<= (unmanagedSourceDirectories in Test, baseDirectory) { (srcDirs, base) => (base / dir / "src/test/scala") +: srcDirs })
  }
  implicit def p2source(p: Project): SourceDepHelper = new SourceDepHelper(p)
  final class SourceDepHelper(p: Project) {
    def dependsOnSource(dir: String): Project =
      p.settings(Dependencies.dependsOnSource(dir):_*)
  }
  
  // compile classpath and classes directory, with provided/optional or scala dependencies
  // specifically for projects that need remote-probe dependencies
  val requiredClasspath = TaskKey[Classpath]("required-classpath")

  def requiredJars(deps: ProjectReference*): Setting[_] = {
    import xsbti.ArtifactInfo._
    import Project.Initialize
    val dependentProjectClassPaths: Seq[Initialize[Task[Seq[File]]]] =
      (deps map { proj => 
        (classDirectory in Compile in proj) map { dir => Seq(dir) }
      })
    val ivyDeps: Initialize[Task[Seq[File]]] =  update map { report =>
      val jars = report.matching(configurationFilter(name = "compile") -- moduleFilter(organization = ScalaOrganization, name = ScalaLibraryID))
      jars
    }
    val localClasses: Initialize[Task[Seq[File]]] = (classDirectory in Compile) map { dir =>
      Seq(dir)
    }
    // JOin everyone
    def joinCp(inits: Seq[Initialize[Task[Seq[File]]]]): Initialize[Task[Seq[File]]] =
      inits reduce { (lhs, rhs) =>
        (lhs zip rhs).flatMap { case (l,r) =>
          l.flatMap[Seq[File]] { files =>
            r.map[Seq[File]] { files2 =>
              files ++ files2
            }
          }
        }
      }
    requiredClasspath <<= joinCp(dependentProjectClassPaths ++ Seq(ivyDeps, localClasses)) map {
      _.classpath
    }
  }
}
