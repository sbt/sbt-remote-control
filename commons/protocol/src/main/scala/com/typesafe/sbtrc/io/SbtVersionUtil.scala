package com.typesafe.sbtrc
package io

import java.io.File
import java.util.Properties

object SbtVersionUtil {

  def findProjectSbtVersion(root: File): Option[String] = {
    val propsFile = new File(root, "project/build.properties")
    val props = new Properties
    val reader = new java.io.FileReader(propsFile)
    try props.load(reader)
    finally reader.close()
    Option(props.getProperty("sbt.version"))
  }

  def findProjectBinarySbtVersion(root: File): Option[String] =
    findProjectSbtVersion(root) map toBinaryVersion


  private val VersionExtractor = new scala.util.matching.Regex("""(\d+\.\d+).*""")
  def toBinaryVersion(version: String): String = {
    version match {
      case VersionExtractor(bv) => bv
      case unknown => sys.error("Bad sbt version number: "+unknown+"!")
    }
  }
}