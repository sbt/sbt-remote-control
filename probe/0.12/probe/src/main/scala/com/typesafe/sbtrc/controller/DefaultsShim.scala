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

  private val nameHandler: RequestHandler = { (origState, ui, params) =>
    val result = extract(origState).get(name)
    (origState, makeResponseParams(protocol.NameResponse(result)))
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

  private val runHandler: RequestHandler = { (origState, ui, params) =>
    val s = runInputTask(run in Compile, origState, args = "")
    (s, makeResponseParams(protocol.RunResponse(success = true,
      task = protocol.TaskNames.run)))
  }

  private val runMainHandler: RequestHandler = { (origState, ui, params) =>
    import ParamsHelper._
    val klass = params.toMap.get("mainClass")
      .map(_.asInstanceOf[String])
      .getOrElse(throw new RuntimeException("need to specify mainClass in params"))
    val s = runInputTask(runMain in Compile, origState, args = klass)
    (s, makeResponseParams(protocol.RunResponse(success = true,
      task = protocol.TaskNames.runMain)))
  }

  private val testHandler: RequestHandler = { (origState, ui, params) =>
    val s1 = addTestListener(origState, ui)
    val (s2, result1) = extract(s1).runTask(test in Test, s1)
    val (s3, outcome) = removeTestListener(s2, ui)
    (s3, makeResponseParams(protocol.TestResponse(outcome)))
  }

  val findHandler: PartialFunction[String, RequestHandler] = {
    case TaskNames.name => nameHandler
    case TaskNames.discoveredMainClasses => discoveredMainClassesHandler
    case TaskNames.watchTransitiveSources => watchTransitiveSourcesHandler
    case TaskNames.compile => compileHandler
    case TaskNames.run => runHandler
    case TaskNames.runMain => runMainHandler
    case TaskNames.test => testHandler
  }
}
