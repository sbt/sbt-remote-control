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
        def parse(s: String): Either[Int, String] =
          try Left(s.toInt)
          catch {
            case _: NumberFormatException => Right(s)
          }
        (parse(aV), parse(bV)) match {
          case (Left(aI), Left(bI)) =>
            aI compare bI
          case (Right(aS), Right(bS)) =>
            aS compare bS
          // alphanumerics like "RC" are lower than 0
          // so that RCs and betas sort before final releases
          case (Left(aI), _) =>
            1
          case (_, Left(bI)) =>
            -1
        }
    }

    // we only care about the first non-zero compare
    // if we get to the end and everything was equal then return 0
    abCompared.dropWhile(_ == 0).headOption.getOrElse(0)
  }
}
