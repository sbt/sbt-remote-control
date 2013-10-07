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
  
  object SbtVersionNeedsUpgraded {
    def unapply(x: String): Boolean = x match {
      case "0.12.0" => true
      case "0.12.1" => true
      case "0.12.2" => true
      case "0.12.3" => true
      case _ => false
    }
  }
  
  def findSafeProjectSbtVersion(root: File): Option[String] = {
    findProjectSbtVersion(root) map {
      // Here we need to upgrade the sbt version to 0.12.4 for our hooks.
      case SbtVersionNeedsUpgraded() => "0.12.4"
      case x => x
    }
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