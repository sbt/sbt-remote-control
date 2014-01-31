package sbt
package server

import xsbti.Position
import xsbti.Severity
import protocol.{
  CompilationFailure,
  ProjectReference
}

class CompileReporter(
  // TODO - Sbt - client and more...
  client: SbtClient,
  project: protocol.ProjectReference,
  maximumErrors: Int,
  log: Logger,
  sourcePositionMapper: Position => Position = { p => p })
  extends LoggerReporter(maximumErrors, log, sourcePositionMapper) {

  override def reset(): Unit = {
    // TODO - Fire event to listeners that errors are reset.
    super.reset()
  }

  override def display(pos: Position, msg: String, severity: Severity) {
    val newPos = sourcePositionMapper(pos)
    val errorMessage =
      protocol.CompilationFailure(
        project,
        newPos,
        severity,
        msg)
    client.send(errorMessage)
    super.display(pos, msg, severity)
  }

  override def printSummary(): Unit = {
    // TODO - Fire event that summary is complete.
    super.printSummary()
  }
}
object CompileReporter {

  def makeShims(state: State): Seq[Setting[_]] = {
    val projects = Project.extract(state).structure.allProjectRefs

    // TODO - Override the derived compiler settings such that
    // our listener is installed on all compilers.
    val extracted = Project.extract(state)
    makeAllReporterSettings(extracted)
  }

  private def makeAllReporterSettings(extracted: Extracted): Seq[Setting[_]] = {
    for {
      setting <- extracted.structure.settings
      scope = setting.key.scope
      if setting.key.key == Keys.compilerReporter.key
      project <- scope.project.toOption.toList
      // TODO - Can we handle other reference types?
      // By this point they should all be unified to ProjectRef/BuildRef.
      if project.isInstanceOf[ProjectRef]
    } yield Keys.compilerReporter in scope := {
      val serverState = ServerState.extract(Keys.state.value)
      val inputs = (Keys.compileInputs in scope).value
      val log = Keys.streams.value.log
      val tmp = Keys.projectInfo
      Some(new CompileReporter(
        serverState.eventListeners,
        SbtToProtocolUtils.projectRefToProtocol(project.asInstanceOf[ProjectRef]),
        inputs.config.maxErrors,
        log,
        inputs.config.sourcePositionMapper))
    }
  }

  private val compileReporter = taskKey[LoggerReporter]("Compilation error reporter.")

  private def makeProjectSettings(project: ProjectRef): Seq[Setting[_]] =
    Seq(
      compileReporter in project := {
        val serverState = ServerState.extract(Keys.state.value)
        new CompileReporter(
          serverState.eventListeners,
          SbtToProtocolUtils.projectRefToProtocol(project),
          100,
          Keys.streams.value.log,
          Keys.compileInputs.value.config.sourcePositionMapper)
      })
}
