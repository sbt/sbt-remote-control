/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import com.typesafe.sbtrc._

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger

class MockSbtProcessFactory extends SbtProcessFactory {
  def creator = new Actor() {
    override def receive = {
      case req: protocol.Request =>
        if (req.sendEvents)
          sender ! protocol.LogEvent(protocol.LogMessage(level = "info", message = "Hello!"))
        req match {
          case protocol.NameRequest(_) =>
            sender ! protocol.NameResponse(Seq(protocol.ProjectInfo(
              protocol.ProjectReference(new java.net.URI("file://test"), "test"),
              "test",
              true,
              Map.empty)))
          case protocol.WatchTransitiveSourcesRequest(_) =>
            sender ! protocol.WatchTransitiveSourcesResponse(Nil)
          case protocol.CompileRequest(_) =>
            sender ! protocol.CompileResponse(success = true)
          case protocol.RunRequest(_, mainClass, _) =>
            sender ! protocol.RunResponse(success = true, mainClass.map(_ => "run-main").getOrElse("run"))
          case protocol.TestRequest(_) =>
            sender ! protocol.TestResponse(outcome = protocol.TestPassed)
          case protocol.CancelRequest =>
            sender ! protocol.CancelResponse
          case req =>
            sender ! protocol.ErrorResponse("GenericRequest not supported here yet")
        }
    }
  }

  val childNum = new AtomicInteger(1)

  override def newChild(actorFactory: ActorRefFactory): ActorRef = {
    actorFactory.actorOf(Props(creator = creator), "mock-sbt-child-" + childNum.getAndIncrement())
  }
}
