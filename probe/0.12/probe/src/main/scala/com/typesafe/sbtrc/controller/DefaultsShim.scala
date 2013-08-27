package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._
import org.scalatools.testing.{ Result => TResult, _ }
import SbtUtil.extract
import SbtUtil.extractWithRef
import SbtUtil.makeAppendSettings
import SbtUtil.reloadWithAppended
import SbtUtil.runInputTask
import protocol.TaskNames
import sbt.ConfigKey.configurationToKey
import sbt.Project.richInitializeTask
import sbt.Scoped.inputScopedToKey
import sbt.Scoped.taskScopedToKey

object DefaultsShim {

  import SbtUtil._
  import protocol.TaskNames

  private def sendEvent(ui: UIContext, id: String, paramsMap: Map[String, Any]): Unit = {
    ui.sendEvent(id, ParamsHelper.fromMap(paramsMap))
  }

  private[sbtrc] def makeResponseParams(specific: protocol.SpecificResponse): Params = {
    ParamsHelper.fromMap(specific.toGeneric.params)
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
            Option(detail.error).map(_.getMessage)).toGeneric.params)
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
    //val s2 = PlaySupport.installPlaySupport(s1, ui)
    val s3 = AtmosSupport.installAtmosSupport(s1, ui)
    s3
  }

  private val nameHandler: RequestHandler = { (origState, ui, params) =>
    val result = extract(origState).get(name)
    // TODO - These are all hacks for now until we have the generic API.
    val hasPlay = controller.isPlayProject(origState)
    val hasConsole = AtmosSupport.isAtmosProject(origState)
    val hasAkka = AkkaSupport.isAkkaProject(origState)

    (origState, makeResponseParams(protocol.NameResponse(result,
      Map("hasPlay" -> hasPlay,
        "hasAkka" -> hasAkka,
        "hasConsole" -> hasConsole))))
  }

  private val discoveredMainClassesHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(discoveredMainClasses in Compile in run, origState)
    (s, makeResponseParams(protocol.DiscoveredMainClassesResponse(names = result)))
  }

  private val watchTransitiveSourcesHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(watchTransitiveSources, origState)
    (s, makeResponseParams(protocol.WatchTransitiveSourcesResponse(files = result)))
  }

  private val compileHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(compile in Compile, origState)
    (s, makeResponseParams(protocol.CompileResponse(success = true)))
  }

  private def makeRunHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    val shimedState = installShims(origState, ui)
    val s = runInputTask(key, shimedState, args = "", Some(ui))
    (origState, makeResponseParams(protocol.RunResponse(success = true,
      task = taskName)))
  }

  private val runHandler: RequestHandler = makeRunHandler(run in Compile, protocol.TaskNames.run)

  private val runAtmosHandler: RequestHandler = makeRunHandler(run in (config("atmos")), protocol.TaskNames.runAtmos)

  private def makeRunMainHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    import ParamsHelper._
    val shimedState = installShims(origState, ui)
    val klass = params.toMap.get("mainClass")
      .map(_.asInstanceOf[String])
      .getOrElse(throw new RuntimeException("need to specify mainClass in params"))
    val s = runInputTask(key, shimedState, args = klass, Some(ui))
    (origState, makeResponseParams(protocol.RunResponse(success = true,
      task = taskName)))
  }

  private val runMainHandler: RequestHandler = makeRunMainHandler(runMain in Compile, protocol.TaskNames.runMain)

  private val runMainAtmosHandler: RequestHandler = makeRunMainHandler(runMain in config("atmos"), protocol.TaskNames.runMainAtmos)

  private val testHandler: RequestHandler = { (origState, ui, params) =>
    val s1 = addTestListener(origState, ui)
    val (s2, result1) = extract(s1).runTask(test in Test, s1)
    val (s3, outcome) = removeTestListener(s2, ui)
    (s3, makeResponseParams(protocol.TestResponse(outcome)))
  }

  private def runCommandHandler(command: String): RequestHandler = { (origState, ui, params) =>
    // TODO - Genericize the command handler.
    val shimedState = addTestListener(origState, ui)
    runCommand(command, shimedState, Some(ui)) -> Params("application/json", "{}")
  }

  val findHandler: PartialFunction[String, RequestHandler] = {
    case TaskNames.name => nameHandler
    case TaskNames.discoveredMainClasses => discoveredMainClassesHandler
    case TaskNames.watchTransitiveSources => watchTransitiveSourcesHandler
    case TaskNames.compile => compileHandler
    case TaskNames.run => runHandler
    case TaskNames.runMain => runMainHandler
    case TaskNames.runAtmos => runAtmosHandler
    case TaskNames.runMainAtmos => runMainAtmosHandler
    case TaskNames.test => testHandler
    case name @ ("eclipse" | "gen-idea") => runCommandHandler(name)

  }
}
