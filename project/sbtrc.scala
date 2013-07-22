import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.SbtGit

object SbtRcBuild {

	def baseVersions: Seq[Setting[_]] = SbtGit.versionWithGit

  def formatPrefs = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(IndentSpaces, 2)
  }

  val typesafeIvyReleases = Resolver.url("typesafe-ivy-private-releases", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
  // TODO - When SBT 0.13 is out we won't need this...
  val typesafeIvySnapshots = Resolver.url("typesafe-ivy-private-snapshots", new URL("http://private-repo.typesafe.com/typesafe/ivy-snapshots/"))(Resolver.ivyStylePatterns)

  def sbtrcDefaults: Seq[Setting[_]] =
    SbtScalariform.scalariformSettings ++
    Seq(
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
      scalaVersion := Dependencies.scalaVersion,
      scalaBinaryVersion := "2.10",
      ScalariformKeys.preferences in Compile := formatPrefs,
      ScalariformKeys.preferences in Test    := formatPrefs
    )

  def sbtShimPluginSettings(sbtVersion: String): Seq[Setting[_]] =
    sbtrcDefaults ++
    Seq(
      scalaVersion := getScalaVersionForSbtVersion(sbtVersion),
      scalaBinaryVersion := sbtVersion,
      sbtPlugin := true,
      publishMavenStyle := false
    )

  def getScalaVersionForSbtVersion(sbt: String) =
    getBinaryVersion(sbt) match {
      case "0.12" => "2.9.2"
      case "0.13" => "2.10.2"
      case _ => sys.error("Unsupported sbt version: " + sbt)
    }
  def getBinaryVersion(version: String): String = {
    val BC = new scala.util.matching.Regex("""(\d+\.\d+).*""")
    // TODO - handle errors?
    version match {
      case BC(bv) => bv
      case _ => version
    }
  }

  def SbtRemoteControlProject(name: String): Project = (
    Project("sbt-rc-" + name, file("sbt-rc") / name)
    settings(sbtrcDefaults:_*)
  )

  def SbtShimPlugin(name: String, sbtVersion: String = Dependencies.sbtPluginVersion): Project = (
    Project("sbt-shim-" + name, file("sbt-shim") / getBinaryVersion(sbtVersion) / name)
    settings(sbtShimPluginSettings(sbtVersion):_*)
  )
  def PropsProject(name: String): Project = (
    Project("sbt-rc-" + name, file(name))
    settings(sbtrcDefaults:_*)
    settings(
        autoScalaLibrary := false
    )
  )
}
