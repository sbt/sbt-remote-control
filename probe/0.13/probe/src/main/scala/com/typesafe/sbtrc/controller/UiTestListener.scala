package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._
import org.scalatools.testing._
import sbt.testing.{ Status => TStatus }
import protocol.TaskNames
import ParamsHelper.p2Helper
import com.typesafe.sbt.ui.{ Context => UIContext }
import sbt.testing.{ Status => TStatus }

/** An sbt test listener that can feed back events over the UI Context. */
class UiTestListener(val ui: UIContext, val oldTask: Task[Seq[TestReportListener]]) extends TestReportListener {

  override def startGroup(name: String): Unit = {}

  var overallOutcome: protocol.TestOutcome = protocol.TestPassed

  override def testEvent(event: TestEvent): Unit = {
    // event.result is just all the detail results folded,
    // we replicate that ourselves below
    for (detail <- event.detail) {
      val outcome = detail.status match {
        case TStatus.Success => protocol.TestPassed
        case TStatus.Error => protocol.TestError
        case TStatus.Failure => protocol.TestFailed
        case TStatus.Skipped => protocol.TestSkipped
        case TStatus.Canceled => protocol.TestSkipped
        case TStatus.Ignored => protocol.TestSkipped
        // TODO - Handle this correctly...
        case TStatus.Pending => protocol.TestSkipped
      }

      // each test group is in its own thread so this has to be
      // synchronized
      synchronized {
        overallOutcome = overallOutcome.combine(outcome)
      }
      sendEvent(ui, "result",
        protocol.TestEvent(detail.fullyQualifiedName,
          None, // No descriptions in new interface?
          outcome,
          Option(detail.throwable).filter(_.isDefined).map(_.get.getMessage)).toGeneric.params)
    }
  }

  private def sendEvent(ui: UIContext, id: String, paramsMap: Map[String, Any]): Unit = {
    ui.sendEvent(id, ParamsHelper.fromMap(paramsMap))
  }

  override def endGroup(name: String, t: Throwable): Unit = {}

  override def endGroup(name: String, result: TestResult.Value): Unit = {}

  override def contentLogger(test: TestDefinition): Option[ContentLogger] = None
}
