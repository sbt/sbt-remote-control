package com.typesafe.sbtrc
package launching

import java.io.File

/**
 * A trait which can create the SBT child process
 * arguments.   Note:  Since we need access to the launcher, for
 * distributed SNAP, we make this something that can be passed in and extended
 * so that we can have a stub implementation.
 */
trait SbtProcessLauncher {
  /**
   * Creates a process which can launch sbt.
   *
   * @param cwd
   *     The directory in which to start sbt (a project directory).
   * @param port
   *     The port with which the sbt process should listen for changes.
   */
  def apply(cwd: File, port: Int): ProcessBuilder

  protected final def quoteCommandLine(argv: Seq[String]): Seq[String] = {
    if (System.getProperty("os.name").toLowerCase.contains("windows")) {
      SbtProcessLauncher.windowsQuoteCommandLine(argv)
    } else {
      argv
    }
  }
}

object SbtProcessLauncher {
  // this is split out to ease unit testing even when not on Windows;
  // use quoteCommandLine in the SbtProcessLauncher trait to auto-adapt
  // to the OS.
  def windowsQuoteCommandLine(argv: Seq[String]): Seq[String] = {
    /* From MSDN CommandLineToArgvW docs.
       * "CommandLineToArgvW has a special interpretation of backslash characters when they
       * are followed by a quotation mark character ("), as follows:
       * . 2n backslashes followed by a quotation mark produce n backslashes followed by a quotation mark.
       * . (2n) + 1 backslashes followed by a quotation mark again produce n backslashes followed by a quotation mark.
       * . n backslashes not followed by a quotation mark simply produce n backslashes."
       *
       * ProcessBuilder adds its own layer, where if an arg contains a space or tab, AND does not already have
       * quotes on the outside, it adds quotes on the outside; but it does not properly quote, just sticks
       * quotes around the outside. By adding our own quotes we should avoid ProcessBuilder's insanity.
       * 
       * See also http://blogs.msdn.com/b/twistylittlepassagesallalike/archive/2011/04/23/everyone-quotes-arguments-the-wrong-way.aspx
       */

    def quote(s: String): String = {
      def backslashes(count: Int): String = """\""" * count
      // For CreateProcess, apparently Windows escapes only '\' and '"'
      // and a backslash before any other char is not special.
      def quoteLoop(cOption: Option[Char], remaining: String, numBackslashes: Int): String = {
        val headOption = remaining.headOption
        val tail = if (remaining.isEmpty) "" else remaining.tail
        cOption match {
          case Some('\\') =>
            // buffer up blocks of backslashes
            quoteLoop(headOption, tail, numBackslashes + 1)
          case Some('"') =>
            // 2n+1 slashes then the quotation mark
            backslashes(1 + numBackslashes * 2) + "\"" + quoteLoop(headOption, tail, numBackslashes = 0)
          case Some(c) =>
            // n backslashes not followed by a quotation mark don't have to be doubled
            backslashes(numBackslashes) + c + quoteLoop(headOption, tail, numBackslashes = 0)
          case None =>
            // we'll add a quote at the end of the string, so we need to double the backslashes
            backslashes(numBackslashes * 2)
        }
      }

      '"' + quoteLoop(s.headOption, if (s.isEmpty) "" else s.tail, 0) + '"'
    }

    // don't quote the first item (the command itself) because ProcessBuilder
    // will normalize the path to it, which will barf if it's quoted.
    // It looks impossible to use a command with quotes in it,
    // though spaces are OK since ProcessBuilder will slap quotes around the outside.
    if (argv.length > 1) {
      Seq(argv.head) ++ (argv.tail map quote)
    } else {
      argv
    }
  }
}
