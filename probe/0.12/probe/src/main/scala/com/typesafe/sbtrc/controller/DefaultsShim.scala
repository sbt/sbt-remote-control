package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._
import org.scalatools.testing.{ Result => TResult, _ }
import SbtUtil.makeAppendSettings
import SbtUtil.reloadWithAppended
import SbtUtil.runInputTask
import protocol.{ TaskNames, TaskParams }
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
  def installShims(origState: State, ui: UIContext, tracePort: Option[Int]): State = {
    val s1 = addTestListener(origState, ui)
    // TODO - Upgrade to shimable play support rather than custom plugin...
    val s2 = PlaySupport.installPlaySupport(s1, ui)
    val s3 = EchoSupport.installEchoSupport(s2, tracePort)
    val s4 = reloadWithUiContext(s3, ui)
    s4
  }

  private val nameHandler: RequestHandler = { (origState, ui, params) =>
    val result = Project.extract(origState).get(name)
    // TODO - These are all hacks for now until we have the generic API.
    val hasPlay = controller.isPlayProject(origState)
    val hasEcho = EchoSupport.isEchoProject(origState)
    val hasAkka = AkkaSupport.isAkkaProject(origState)

    (origState, makeResponseParams(protocol.NameResponse(result,
      Map("hasPlay" -> hasPlay,
        "hasAkka" -> hasAkka,
        "hasEcho" -> hasEcho))))
  }

  private def withUiContext[T](state: State, ui: UIContext)(f: State => (State, T)): (State, T) = {
    val newState = reloadWithUiContext(state, ui)
    f(newState)
  }

  private def runTask[T](state: State, ui: UIContext, key: TaskKey[T])(f: T => protocol.SpecificResponse): (State, Params) = {
    withUiContext(state, ui) { newState =>
      val (s, result) = Project.extract(newState).runTask(key, newState)
      (s, makeResponseParams(f(result)))
    }
  }

  private val mainClassHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Running `mainClass` task.")
    runTask(origState, ui, mainClass in Compile in run) { result =>
      protocol.MainClassResponse(name = result)
    }
  }

  private val discoveredMainClassesHandler: RequestHandler = { (origState, ui, params) =>
    runTask(origState, ui, discoveredMainClasses in Compile in run)(result => protocol.DiscoveredMainClassesResponse(result))
  }

  private val watchTransitiveSourcesHandler: RequestHandler = { (origState, ui, params) =>
    runTask(origState, ui, watchTransitiveSources)(result => protocol.WatchTransitiveSourcesResponse(files = result))
  }

  private val compileHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Compiling the project.")
    runTask(origState, ui, compile in Compile)(result => protocol.CompileResponse(success = true))
  }

  private def makeRunHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    import ParamsHelper._
    val tracePort = TaskParams.tracePort(params.toMap)
    val shimedState = installShims(origState, ui, tracePort)
    val s = runInputTask(key, shimedState, args = "")
    (s, makeResponseParams(protocol.RunResponse(success = true, task = taskName)))
  }

  private val runHandler: RequestHandler = makeRunHandler(run in Compile, protocol.TaskNames.run)

  private val runEchoHandler: RequestHandler = makeRunHandler(run in (config("echo")), protocol.TaskNames.runEcho)

  private def makeRunMainHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    import ParamsHelper._
    val tracePort = TaskParams.tracePort(params.toMap)
    val shimedState = installShims(origState, ui, tracePort)
    val klass = TaskParams.mainClass(params.toMap)
      .getOrElse(throw new RuntimeException("need to specify mainClass in params"))
    val s = runInputTask(key, shimedState, args = klass)
    (s, makeResponseParams(protocol.RunResponse(success = true, task = taskName)))
  }

  private val runMainHandler: RequestHandler = makeRunMainHandler(runMain in Compile, protocol.TaskNames.runMain)

  private val runMainEchoHandler: RequestHandler = makeRunMainHandler(runMain in config("echo"), protocol.TaskNames.runMainEcho)

  private val testHandler: RequestHandler = { (origState, ui, params) =>
    val shimedState = installShims(origState, ui, tracePort = None)
    val (s2, result1) = Project.extract(shimedState).runTask(test in Test, shimedState)
    val (s3, outcome) = removeTestListener(s2, ui)
    (s3, makeResponseParams(protocol.TestResponse(outcome)))
  }

  private def runCommandHandler(command: String): RequestHandler = { (origState, ui, params) =>
    // TODO - Genericize the command handler.
    val shimedState = installShims(origState, ui, tracePort = None)
    SbtUtil.runCommand(command, shimedState) -> Params("application/json", "{}")
  }

  val findHandler: PartialFunction[String, RequestHandler] = {
    case TaskNames.name => nameHandler
    case TaskNames.mainClass => mainClassHandler
    case TaskNames.discoveredMainClasses => discoveredMainClassesHandler
    case TaskNames.watchTransitiveSources => watchTransitiveSourcesHandler
    case TaskNames.compile => compileHandler
    case TaskNames.run => runHandler
    case TaskNames.runMain => runMainHandler
    case TaskNames.runEcho => runEchoHandler
    case TaskNames.runMainEcho => runMainEchoHandler
    case TaskNames.test => testHandler
    case name @ ("eclipse" | "gen-idea") => runCommandHandler(name)

  }
}
