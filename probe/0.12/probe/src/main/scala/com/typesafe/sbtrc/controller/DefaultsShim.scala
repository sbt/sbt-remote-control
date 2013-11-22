package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext }
import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._
import org.scalatools.testing.{ Result => TResult, _ }
import SbtUtil.extract
import SbtUtil.extractWithRef
import SbtUtil.makeAppendSettings
import SbtUtil.reloadWithAppended
import SbtUtil.runInputTask
import sbt.ConfigKey.configurationToKey
import sbt.Project.richInitializeTask
import sbt.Scoped.inputScopedToKey
import sbt.Scoped.taskScopedToKey
import com.typesafe.sbtrc.PoorManDebug

object DefaultsShim {

  import SbtUtil._

  private def sendEvent[T <: protocol.Event](ui: UIContext, id: String, event: T)(implicit struct: protocol.RawStructure[T]): Unit = {
    ui.sendEvent(id, struct(event))
  }

  private class OurTestListener(val ui: UIContext, val oldTask: Task[Seq[TestReportListener]]) extends TestReportListener {

    override def startGroup(name: String): Unit = {}

    var overallOutcome: protocol.TestOutcome = protocol.TestPassed

    override def testEvent(event: TestEvent): Unit = {
      // event.result is just all the detail results folded,
      // we replicate that ourselves below
      for (detail <- event.detail) {
        val outcome = detail.result match {
          case TResult.Success => protocol.TestPassed
          case TResult.Error => protocol.TestError
          case TResult.Failure => protocol.TestFailed
          case TResult.Skipped => protocol.TestSkipped
        }

        // each test group is in its own thread so this has to be
        // synchronized
        synchronized {
          overallOutcome = overallOutcome.combine(outcome)
        }

        sendEvent(ui, "result",
          protocol.TestEvent(detail.testName,
            Option(detail.description),
            outcome,
            Option(detail.error).map(_.getMessage)))
      }
    }

    override def endGroup(name: String, t: Throwable): Unit = {}

    override def endGroup(name: String, result: TestResult.Value): Unit = {}

    override def contentLogger(test: TestDefinition): Option[ContentLogger] = None
  }

  private val listenersKey = testListeners in Test

  private def addTestListener(state: State, ui: UIContext): State = {
    val (extracted, ref) = extractWithRef(state)
    val ourListener = new OurTestListener(ui, extracted.get(listenersKey))

    val settings = makeAppendSettings(Seq(listenersKey <<= (listenersKey) map { listeners =>
      listeners :+ ourListener
    }), ref, extracted)

    reloadWithAppended(state, settings)
  }

  private def removeTestListener(state: State, ui: UIContext): (State, protocol.TestOutcome) = {
    val ref = Project.extract(state).currentRef
    val extracted = Extracted(Project.structure(state), Project.session(state), ref)(Project.showFullKey)

    val (s1, listeners) = extracted.runTask(listenersKey, state)

    val ours = listeners.flatMap({
      case l: OurTestListener if l.ui eq ui => Seq(l)
      case whatever => Seq.empty[OurTestListener]
    }).headOption
      .getOrElse(throw new RuntimeException("Our test listener wasn't installed!"))

    // put back the original listener task
    val settings = makeAppendSettings(Seq(Project.setting(listenersKey, Project.value(ours.oldTask))), ref, extracted)

    (reloadWithAppended(s1, settings), ours.overallOutcome)
  }

  /** This installs all of our shim hooks into the project. */
  def installShims(origState: State, ui: UIContext): State = {
    val s1 = addTestListener(origState, ui)
    // TODO - Upgrade to shimable play support rather than custom plugin...
    val s2 = PlaySupport.installPlaySupport(s1, ui)
    val s3 = AtmosSupport.installAtmosSupport(s2, ui)
    s3
  }

  private val nameHandler: RequestHandler = { (origState, ui, request) =>
    PoorManDebug.debug("Extracting name and capabilities of this build.")

    // TODO - Do this for every major project
    val extracted = extract(origState)
    val results =
      for {
        ref <- extracted.structure.allProjectRefs
      } yield {
        val name = extracted.get(sbt.Keys.name in ref)
        val hasPlay = controller.isPlayProject(origState, Some(ref))
        val hasConsole = AtmosSupport.isAtmosProject(origState, Some(ref))
        val hasAkka = AkkaSupport.isAkkaProject(origState, Some(ref))
        protocol.ProjectInfo(
          SbtToProtocolUtils.projectRefToProtocol(ref),
          name,
          true,
          Map("hasPlay" -> hasPlay,
            "hasAkka" -> hasAkka,
            "hasConsole" -> hasConsole))
      }
    (origState, protocol.NameResponse(results))
  }

  private val mainClassHandler: RequestHandler = {
    case (origState, ui, protocol.MainClassRequest(_, ref)) =>
      PoorManDebug.debug("Running `mainClass` task.")
      val extracted = extract(origState)

      val refs = ref.map(r => Seq(SbtToProtocolUtils.projectRefFromProtocol(r))).getOrElse(extracted.structure.allProjectRefs)
      val results =
        for (ref <- refs) yield {
          val (_, mc) = extracted.runTask(mainClass in Compile in run in ref, origState)
          val (_, discoveredMc) = extracted.runTask(discoveredMainClasses in Compile in run in ref, origState)
          protocol.DiscoveredMainClasses(
            SbtToProtocolUtils.projectRefToProtocol(ref),
            mainClasses = discoveredMc,
            defaultMainClass = mc)
        }
      (origState, protocol.MainClassResponse(results))
  }

  private val watchTransitiveSourcesHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(watchTransitiveSources, origState)
    (s, protocol.WatchTransitiveSourcesResponse(files = result))
  }

  private val compileHandler: RequestHandler = {
    case (origState, ui, protocol.CompileRequest(_, pref)) =>
      PoorManDebug.debug("Compiling the project: " + pref)
      // TODO - handle aggregation....
      val extracted = extract(origState)
      val refs = pref match {
        case Some(r) => Seq(SbtToProtocolUtils.projectRefFromProtocol(r))
        case _ => extracted.structure.allProjectRefs
      }
      // Fold over all the refs and run the compile task.
      val (state, results) =
        ((origState -> Vector.empty[protocol.CompileResult]) /: refs) {
          case ((state, results), ref) =>
            val key = compile in Compile in ref
            val pref = SbtToProtocolUtils.projectRefToProtocol(ref)
            try {
              val (s, result) = extracted.runTask(key, state)
              state -> (results :+ protocol.CompileResult(pref, true))
            } catch {
              case e: Exception =>
                // TODO - just catch task exceptions
                state -> (results :+ protocol.CompileResult(pref, false))
            }
        }
      (state, protocol.CompileResponse(results))
  }

  private def makeRunHandler[T](key: sbt.InputKey[T], taskName: String): RequestHandler = {
    case (origState, ui, protocol.RunRequest(_, ref, _, _)) =>
      PoorManDebug.debug("Invoking the run task in " + key.scope.config)
      val key2 = ref match {
        case Some(r) => key in SbtToProtocolUtils.projectRefFromProtocol(r)
        case _ => key
      }
      val shimedState = installShims(origState, ui)
      val s = runInputTask(key2, shimedState, args = "", Some(ui))
      (origState, protocol.RunResponse(success = true,
        task = taskName))
  }

  private val runHandler: RequestHandler = makeRunHandler(run in Compile, "run")

  private val runAtmosHandler: RequestHandler = makeRunHandler(run in (config("atmos")), "atmos:run")

  private def makeRunMainHandler[T](key: sbt.InputKey[T], taskName: String): RequestHandler = {
    case (origState, ui, protocol.RunRequest(_, ref, Some(klass), _)) =>
      PoorManDebug.debug("Invoking the run-main task in " + key.scope.config)
      val key2: sbt.InputKey[T] = ref match {
        case Some(r) => key in SbtToProtocolUtils.projectRefFromProtocol(r)
        case _ => key
      }
      // Note: For now this is safe. In the future, let's just not cast 30 bajillion times.
      val shimedState = installShims(origState, ui)
      val s = runInputTask(key2, shimedState, args = klass, Some(ui))
      (origState, protocol.RunResponse(success = true, task = taskName))
  }
  private val runMainHandler: RequestHandler = makeRunMainHandler(runMain in Compile, "run-main")

  private val runMainAtmosHandler: RequestHandler = makeRunMainHandler(runMain in config("atmos"), "atmos:run-main")

  private val testHandler: RequestHandler = { (origState, ui, params) =>
    val s1 = addTestListener(origState, ui)
    val (s2, result1) = extract(s1).runTask(test in Test, s1)
    val (s3, outcome) = removeTestListener(s2, ui)
    (s3, protocol.TestResponse(outcome))
  }

  private def runCommandHandler(command: String): RequestHandler = { (origState, ui, request) =>
    // TODO - Genericize the command handler.
    val shimedState = installShims(origState, ui)
    // TODO - Command response
    // Params("application/json", "{}")
    runCommand(command, shimedState, Some(ui)) -> protocol.ExecuteCommandResponse()
  }

  val findHandler: PartialFunction[protocol.Request, RequestHandler] = {
    case _: protocol.NameRequest => nameHandler
    case _: protocol.MainClassRequest => mainClassHandler
    case _: protocol.WatchTransitiveSourcesRequest => watchTransitiveSourcesHandler
    case _: protocol.CompileRequest => compileHandler
    case protocol.RunRequest(_, _, None, false) => runHandler
    case protocol.RunRequest(_, _, Some(_), false) => runMainHandler
    case protocol.RunRequest(_, _, None, true) => runAtmosHandler
    case protocol.RunRequest(_, _, Some(_), true) => runMainAtmosHandler
    case _: protocol.TestRequest => testHandler
    case protocol.ExecuteCommandRequest(name, _) => runCommandHandler(name)

  }
}
