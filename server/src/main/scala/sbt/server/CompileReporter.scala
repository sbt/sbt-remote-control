package sbt
package server

import xsbti.Position
import xsbti.Severity
import sbt.serialization._

class CompileReporter(
  sendEventService: SendEventService,
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
        SbtToProtocolUtils.positionToProtocol(pos),
        severity,
        msg)
    sendEventService.sendEvent(errorMessage)
    super.display(pos, msg, severity)
  }

  override def printSummary(): Unit = {
    // TODO - Fire event that summary is complete.
    super.printSummary();
  }
}
object CompileReporter {

  // Don't pass anything in here that's "internal" because we
  // should be moving this code into the default sbt compile task,
  // and it won't be able to use internals. You probably have to
  // add anything you need to UIContext.
  def makeShims(state: State): Seq[Setting[_]] = {
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
      val sendEventService = UIKeys.sendEventService.value
      val inputs = (Keys.compileInputs in scope).value
      val log = Keys.streams.value.log
      val tmp = Keys.projectInfo
      Some(new CompileReporter(
        sendEventService,
        SbtToProtocolUtils.projectRefToProtocol(project.asInstanceOf[ProjectRef]),
        inputs.config.maxErrors,
        log,
        inputs.config.sourcePositionMapper))
    }
  }
}
