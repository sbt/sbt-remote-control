import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.SbtGit
import Dependencies.getScalaVersionForSbtVersion

object SbtRcBuild {

	def baseVersions: Seq[Setting[_]] = SbtGit.versionWithGit

  def formatPrefs = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(IndentSpaces, 2)
  }
  val typesafeMvnReleases = "typesafe-mvn-private-releases" at "http://private-repo.typesafe.com/typesafe/mvn-releases/"
  val typesafeIvyReleases = Resolver.url("typesafe-ivy-private-releases", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
  // TODO - When SBT 0.13 is out we won't need this...
  val typesafeIvySnapshots = Resolver.url("typesafe-ivy-private-snapshots", new URL("http://private-repo.typesafe.com/typesafe/ivy-snapshots/"))(Resolver.ivyStylePatterns)

  import sbtassembly.Plugin._
  import sbtassembly.Plugin.AssemblyKeys._
  lazy val RepackageDep = config("repackage-dep")

  def sbtrcDefaults: Seq[Setting[_]] =
    SbtScalariform.scalariformSettings ++
    Seq(
      // TODO - Move everything ot com.typesafe.sbt
      organization := "com.typesafe.sbtrc",
      version <<= version in ThisBuild,
      crossPaths := false,
      resolvers += "typesafe-mvn-releases" at "http://repo.typesafe.com/typesafe/releases/",
      resolvers += Resolver.url("typesafe-ivy-releases", new URL("http://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns),
      // TODO - This won't be needed when SBT 0.13 is released...
      resolvers += typesafeIvyReleases,
      resolvers += typesafeIvySnapshots,
      // TODO - Publish to ivy for sbt plugins, maven central otherwise?
      publishTo := Some(typesafeIvyReleases),
      publishMavenStyle := false,
      scalacOptions <<= (scalaVersion) map { sv =>
        Seq("-unchecked", "-deprecation") ++
          { if (sv.startsWith("2.9")) Seq.empty else Seq("-feature") }
      },
      javacOptions in Compile := Seq("-target", "1.6", "-source", "1.6"),
      javacOptions in (Compile, doc) := Seq("-source", "1.6"),
      libraryDependencies += Dependencies.junitInterface % "test",
      // Scaladoc is slow as molasses.
      Keys.publishArtifact in (Compile, packageDoc) := false,
      scalaVersion := Dependencies.scalaVersion,
      scalaBinaryVersion <<= scalaVersion apply { sv =>
        CrossVersion.binaryScalaVersion(sv)
      },
      ScalariformKeys.preferences in Compile := formatPrefs,
      ScalariformKeys.preferences in Test    := formatPrefs
    )
  def sbtrcRepackagedDefaults: Seq[Setting[_]] =
    Seq(
      organization := "com.typesafe.sbt",
      publishMavenStyle := true,
      publishTo := Some(typesafeMvnReleases),
      pomIncludeRepository := { _ => false },
      managedClasspath in RepackageDep := {
        // TODO - Anything else we don't include?
        def isExcluded(m: ModuleID): Boolean =
          (m.organization == "org.scala-lang")
        Classpaths.managedJars(RepackageDep, classpathTypes.value, update.value) filterNot { data =>
          // TODO - Here we're filtering out scala things, although we want them as dependencies...
          data get Keys.moduleID.key map isExcluded getOrElse false
        }
      },
      assembleArtifact in packageScala := false,
      fullClasspath in assembly <<= managedClasspath in RepackageDep,
      packageBin in Compile <<= (assembly, artifactPath in packageBin in Compile) map {
        (assembled, packaged) => IO.copyFile(assembled, packaged, false); packaged
      }
    )

  def sbtProbeSettings(sbtVersion: String): Seq[Setting[_]] =
    Seq(
      scalaVersion := getScalaVersionForSbtVersion(sbtVersion),
      Keys.sbtVersion := sbtVersion
    )

  def sbtShimPluginSettings(sbtVersion: String): Seq[Setting[_]] =
    Seq(
      sbtPlugin := true,
      publishMavenStyle := false,
      // TODO - Whatever hacks we need so our definition is in whatever build definition.
      Keys.sbtVersion := sbtVersion,
      sbtBinaryVersion <<= Keys.sbtVersion apply CrossVersion.binarySbtVersion
    )

  def SbtRemoteControlProject(name: String): Project = (
    Project(name, file(name))
    settings(sbtrcDefaults:_*)
  )

  def SbtRemoteControlRepackagedProject(name: String): Project = (
    SbtRemoteControlProject(name)
    settings((assemblySettings) :_*)
    settings(sbtrcRepackagedDefaults:_*)
    configs(RepackageDep)
  )

  def SbtProbeProject(name: String, sbtVersion: String): Project = {
    val sbtBinaryVersion = Dependencies.crossSbtVersion(sbtVersion)
    val scrubNameForId =
      sbtBinaryVersion.replaceAll("""[\W+\-]""", "-")
    (
      Project(name + "-"+scrubNameForId, file(name))
      settings(sbtrcDefaults:_*)
      settings(sbtProbeSettings(sbtVersion): _*)
    )
  }

  def SbtShimPlugin(name: String, sbtVersion: String): Project = (
    SbtProbeProject(name, sbtVersion)
    settings(sbtShimPluginSettings(sbtVersion):_*)
  )
  def PropsProject(name: String): Project = (
    Project("sbt-rc-" + name, file(name))
    settings(sbtrcDefaults:_*)
    settings(
        autoScalaLibrary := false
    )
  )

  def noCrossVersioning: Seq[Setting[_]] =
    Seq(
       Keys.crossVersion := CrossVersion.Disabled,
       Keys.projectID <<=  Keys.projectID apply { id =>
         id.copy(extraAttributes = Map.empty)
        }
    )
}
