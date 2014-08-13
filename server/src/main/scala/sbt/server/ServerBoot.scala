package sbt
package server

import BasicCommands._
import BasicCommandStrings._
import CommandStrings._
import BuiltinCommands._
import CommandUtil._
import Project.LoadAction.{ Value => LoadActionValue }
import Project.loadActionParser
import complete.DefaultParsers._
import sbt.StateOps

/**
 * Represents overrides of the project loading commands that are required for appropriate server
 * usage.
 *
 * For example:
 * - We do not want to hit System.in on load failure for confirmation of retry, but instead hit a client.
 * - We may want to "hook" the notion of `reload plugins`/`reload return` for special handling (not implemented).
 */
object ServerBootCommand {

  /** A new load failed command which handles the server requirements */
  val serverLoadFailed = Command(LoadFailed)(loadProjectParser)(doServerLoadFailed)

  /** List of commands which override sbt's default commands. */
  def commandOverrides = Seq(serverLoadFailed)

  def isOverriden(cmd: Command): Boolean =
    cmd == loadFailed

  /** Actual does the failing to load for the sbt server. */
  private[this] def doServerLoadFailed(s: State, action: String): State = {
    s.log.error("Failed to load project.")
    // TODO we need to notify ReadOnlyServerEngine and
    // have it either take care of exiting, or enter
    // some sort of retry loop, or whatever it will do.
    s.exit(ok = false)
  }

  // TODO - Copied out of BuiltInCommands
  private[this] def loadProjectParser = (s: State) => matched(loadActionParser)
}