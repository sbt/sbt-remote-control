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
import com.typesafe.sbt.ui.{ Context => UIContext }

/** An sbt test listener that can feed back events over the UI Context. */
class UiTestListener(val ui: UIContext) extends TestReportListener {

  override def startGroup(name: String): Unit = {}
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

  private def sendEvent[T](ui: UIContext, id: String, msg: T)(implicit struct: protocol.RawStructure[T]): Unit = {
    sendEventRaw(ui, id, struct(msg))
  }
  private def sendEventRaw(ui: UIContext, id: String, paramsMap: Map[String, Any]): Unit = {
    ui.sendEvent(id, paramsMap)
  }
}
