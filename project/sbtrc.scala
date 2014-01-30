import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.SbtGit
import Dependencies.getScalaVersionForSbtVersion
import sbtbuildinfo.Plugin._ 

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
      // Scaladoc is slow as molasses.
      Keys.publishArtifact in (Compile, packageDoc) := false,
      scalaVersion := Dependencies.scalaVersion,
      scalaBinaryVersion <<= scalaVersion apply { sv =>
        CrossVersion.binaryScalaVersion(sv)
      },
      ScalariformKeys.preferences in Compile := formatPrefs,
      ScalariformKeys.preferences in Test    := formatPrefs
    )

  def sbtProbeSettings(sbtVersion: String): Seq[Setting[_]] =
    buildInfoSettings ++
    Seq(
      scalaVersion := getScalaVersionForSbtVersion(sbtVersion),
      Keys.sbtVersion := sbtVersion,
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys ++= Seq[BuildInfoKey](
        "supportedAkkaVersionSbt012" -> Dependencies.sbt012AtmosSupportedAkkaVersion,
        "supportedPlayVersionSbt012" -> Dependencies.sbt012AtmosSupportedPlayVersion,
        "supportedAkkaVersionSbt013" -> Dependencies.sbt013AtmosSupportedAkkaVersion,
        "supportedPlayVersionSbt013" -> Dependencies.sbt013AtmosSupportedPlayVersion),
      buildInfoPackage := "com.typesafe.sbtrc"
    )

  def sbtShimPluginSettings(sbtVersion: String): Seq[Setting[_]] =
    Seq(
      sbtPlugin := true,
      publishMavenStyle := false,
      Keys.sbtVersion := sbtVersion,
      sbtBinaryVersion <<= Keys.sbtVersion apply CrossVersion.binarySbtVersion,
      // Hacked so we get the right dependnecies...
      allDependencies <<= (Keys.projectDependencies, Keys.libraryDependencies, Keys.sbtVersion) map { (pd, ld, sv) =>
        val base = pd ++ ld
        (Dependencies.sbtOrg % "sbt" % sv % Provided.name) +: base
      }
    )

  def SbtRemoteControlProject(name: String): Project = (
    Project("sbt-rc-" + name, file(name))
    settings(sbtrcDefaults:_*)
  )

  def SbtProbeProject(name: String, sbtVersion: String): Project = {
    val sbtBinaryVersion = CrossVersion.binarySbtVersion(sbtVersion)
    val scrubNameForId =
      sbtBinaryVersion.replaceAll("""[\W+\-]""", "-")
    (
      Project("sbt-rc-" + name + "-"+scrubNameForId, file("probe") / sbtBinaryVersion / name)
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
