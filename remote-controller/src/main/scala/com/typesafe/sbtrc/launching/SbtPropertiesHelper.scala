package com.typesafe.sbtrc.launching

import java.io.File

object RepoHelper {
  def mvn(name: String, file: File): SbtPropertiesHelper.Repository =
    (name, file, None)

  def ivy(name: String, file: File): SbtPropertiesHelper.Repository =
    (name, file, Some("[organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]"))
}

object SbtPropertiesHelper {
  type Repository = (String, File, Option[String])
  // TODO - Allow custom repositories....
  def makePropertiesFile(
    file: File,
    sbtVersion: String,
    extraJars: Seq[File] = Nil,
    optionalRepositories: Seq[Repository] = Seq.empty): Unit = {

    def makeRepositoryString(tuple: (String, File, Option[String])): String = tuple match {
      case (name, uri, None) => s"$name: ${uri.toURI}"
      case (name, uri, Some(pattern)) => s"$name: ${uri.toURI}, $pattern"
    }
    // TODO - Users should specify the *complete* definition....
    // This is just a hack for us right now...
    val optionalRepositoriesStrings = optionalRepositories map makeRepositoryString
    val resourcesProperty = if (extraJars.nonEmpty)
      "resources: " + (extraJars map (_.getCanonicalPath) mkString ",")
    else
      ""

    val writer = new java.io.BufferedWriter(new java.io.FileWriter(file))
    try {
      writer.write(s"""
[scala]
  version: auto

[app]
  org: org.scala-sbt
  name: sbt
  version: ${sbtVersion}
  class: sbt.xMain
  components: xsbti,extra
  cross-versioned: false
  ${resourcesProperty}

[repositories]
  local
  ${optionalRepositoriesStrings mkString "\n  "}
  typesafe-ivy-releases: http://repo.typesafe.com/typesafe/ivy-releases/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
  typesafe-ivy-snapshots: http://repo.typesafe.com/typesafe/ivy-snapshots/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
  maven-central

[boot]
 directory: $${sbt.boot.directory-$${sbt.global.base-$${user.home}/.sbt}/boot/}

[ivy]
  ivy-home: $${sbt.ivy.home-$${user.home}/.ivy2/}
  checksums: $${sbt.checksums-sha1,md5}
  override-build-repos: $${sbt.override.build.repos-false}
  repository-config: $${sbt.repository.config-$${sbt.global.base-$${user.home}/.sbt}/repositories}
""")
    } finally {
      writer.close()
    }
  }
}
