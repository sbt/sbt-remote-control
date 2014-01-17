package sbt
package server

import play.api.libs.json.Format

object TestShims {

  val serverTestListener = taskKey[TestReportListener]("Global test listener for the sbt server.")
  val testShimSettings: Seq[Setting[_]] =
    Seq(
      serverTestListener in Global := {
        val state = sbt.Keys.state.value
        val listener = ServerState.extract(state).eventListeners
        new ServerTestListener(listener)
      },
      sbt.Keys.testListeners in Test in Global += (serverTestListener in Global).value)
  def makeShims(state: State): Seq[Setting[_]] =
    testShimSettings
}

import testing.{ Logger => TLogger, Event => TEvent, Status => TStatus }

class ServerTestListener(val client: SbtClient) extends TestReportListener {
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
      sendEvent(
        protocol.TestEvent(detail.fullyQualifiedName,
          None, // No descriptions in new interface?
          outcome,
          Option(detail.throwable).filter(_.isDefined).map(_.get.getMessage)))
    }
  }

  private def sendEvent[T: Format](msg: T): Unit = {
    client.send(msg)
  }

  override def endGroup(name: String, t: Throwable): Unit = {}

  override def endGroup(name: String, result: TestResult.Value): Unit = {}

  override def contentLogger(test: TestDefinition): Option[ContentLogger] = None
}