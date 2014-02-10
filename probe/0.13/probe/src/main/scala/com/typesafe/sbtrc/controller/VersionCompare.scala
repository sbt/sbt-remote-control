package com.typesafe.sbtrc.controller

object VersionCompare {
  // cut-and-paste hack from webjars. All this junk
  // is supposed to go away with newer sbt.
  def apply(a: String, b: String): Int = {
    val aParts = a.split(Array('.', '-'))
    val bParts = b.split(Array('.', '-'))

    // figure out the longest one and pad each with a string 0 until the sizes match
    val longest = aParts.length max bParts.length

    val paddedAParts = aParts.padTo(longest, "0")
    val paddedBParts = bParts.padTo(longest, "0")

    // combine the two arrays into one with tuples
    val abParts = paddedAParts zip paddedBParts

    // compare all the parts as ints
    // todo: could be optimized
    val abCompared = abParts.map {
      case (aV, bV) =>
        try {
          aV.toInt compare bV.toInt
        } catch {
          // if we can't compare because it's not an int then just set this one to 1
          case e: NumberFormatException => 1
        }
    }

    // we only care about the first non-zero compare
    // if we get to the end and everything was equal then return 0
    abCompared.dropWhile(_ == 0).headOption.getOrElse(0)
  }
}
