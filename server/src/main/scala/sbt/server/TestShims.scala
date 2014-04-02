package sbt
package server

import play.api.libs.json.Format

object TestShims {

  val serverTestListener = taskKey[TestReportListener]("Global test listener for the sbt server.")
  val testShimSettings: Seq[Setting[_]] =
    Seq(
      serverTestListener in Global := {
        val context = UIContext.uiContext.value
        new ServerTestListener(context)
      },
      sbt.Keys.testListeners in Test in Global += (serverTestListener in Global).value)

  // Don't pass anything in here that's "internal" because we
  // should be moving this code into the default sbt test task,
  // and it won't be able to use internals. You probably have to
  // add anything you need to UIContext.
  def makeShims(state: State): Seq[Setting[_]] =
    testShimSettings
}

import testing.{ Logger => TLogger, Event => TEvent, Status => TStatus }

class ServerTestListener(val context: UIContext) extends TestReportListener {
  override def startGroup(name: String): Unit = {}

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
      context.sendEvent(
        protocol.TestEvent(context.taskId,
          detail.fullyQualifiedName,
          None, // No descriptions in new interface?
          outcome,
          Option(detail.throwable).filter(_.isDefined).map(_.get.getMessage)))
    }
  }

  override def endGroup(name: String, t: Throwable): Unit = {}

  override def endGroup(name: String, result: TestResult.Value): Unit = {}

  override def contentLogger(test: TestDefinition): Option[ContentLogger] = None
}