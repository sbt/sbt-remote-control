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
 * - We want to "hook" the notion of `reload plugins`/`reload return` for special handling.
 */
object ServerBootCommand {

  /** A new load failed command which handles the server requirements */
  private def serverLoadFailed(eventSink: JsonSink[protocol.ExecutionEngineEvent], engine: ServerEngine) =
    Command(LoadFailed)(loadProjectParser) { (state, action) =>
      state.log.error("Failed to load project.")
      eventSink.send(protocol.BuildFailedToLoad())
      state.copy(remainingCommands = Seq(engine.HandleNextRebootRequest), next = State.Continue)
    }

  private def projectReload(engine: ServerEngine) = {
    Command(LoadProject)(_ => Project.loadActionParser) { (state, action) =>
      action match {
        case Project.LoadAction.Current =>
          engine.installBuildHooks(BuiltinCommands.doLoadProject(state, action))
        // TODO : Detect setting changes and fire an event
        case _ =>
          throw new IllegalArgumentException("'reload' command is not supported for plugins.")
      }
    }
  }

  /** List of commands which override sbt's default commands. */
  def commandOverrides(engine: ServerEngine, eventSink: JsonSink[protocol.ExecutionEngineEvent]): Seq[Command] =
    Seq(serverLoadFailed(eventSink, engine), projectReload(engine))

  val overriddenCommands = Seq(loadFailed, loadProject)

  def isOverridden(cmd: Command): Boolean = overriddenCommands.contains(cmd)

  // TODO - Copied out of BuiltInCommands
  private[this] def loadProjectParser = (s: State) => matched(loadActionParser)
}
