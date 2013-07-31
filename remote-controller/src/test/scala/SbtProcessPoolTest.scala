/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import java.io.File
import akka.actor._
import akka.dispatch._
import akka.util._
import scala.concurrent._
import scala.concurrent.duration._
import akka.pattern._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.TimeUnit

class SbtProcessPoolTest {

  class TestActor extends Actor {

    val pool = context.actorOf(Props(new ChildPool(new MockSbtProcessFactory)), "pool")

    var replyTo: Option[ActorRef] = None
    var recording = Vector.empty[Any]

    def recorder: Receive = {
      case grant: SbtGranted if grant.reservation.id == "no-notifications" =>
        recording :+= grant
        for (r <- replyTo)
          r ! recording
      case whatever =>
        recording :+= whatever
    }

    override def receive = {
      case "test" =>
        pool ! SubscribeSbts(self)
        for (i <- 1 to 5)
          pool ! RequestAnSbt(SbtReservation(i.toString, "Request " + i))

        for (i <- 1 to 5)
          pool ! ReleaseAnSbt(i.toString)

        pool ! UnsubscribeSbts(self)
        pool ! RequestAnSbt(SbtReservation("no-notifications", "No notifications"))
        pool ! ReleaseAnSbt("no-notifications")

        replyTo = Some(sender)
        context.become(recorder)
    }
  }

  @Test
  def testPool(): Unit = {
    val system = ActorSystem("testPoolSystem")

    val actor = system.actorOf(Props(new TestActor), "child-pool-test")

    val expectations = Seq[PartialFunction[Any, Unit]](
      { case SbtReservationsChanged(x) if x.isEmpty => },
      { case SbtGranted(reservation) if reservation.id == "1" => },
      { case SbtReservationsChanged(x) if x.size == 1 => },
      { case SbtGranted(reservation) if reservation.id == "2" => },
      { case SbtReservationsChanged(x) if x.size == 2 => },
      { case SbtGranted(reservation) if reservation.id == "3" => },
      { case SbtReservationsChanged(x) if x.size == 3 => },
      { case SbtGranted(reservation) if reservation.id == "4" => },
      { case SbtReservationsChanged(x) if x.size == 3 => }, // pool max size is 3
      { case SbtGranted(reservation) if reservation.id == "5" => },
      { case SbtReservationsChanged(x) if x.size == 3 => },
      { case SbtReservationsChanged(x) if x.size == 2 => },
      { case SbtReservationsChanged(x) if x.size == 1 => },
      { case SbtReservationsChanged(x) if x.size == 0 => },
      { case SbtGranted(reservation) if reservation.id == "no-notifications" => })

    implicit val timeout = Timeout(5, TimeUnit.SECONDS)
    Await.result(actor ? "test", timeout.duration) match {
      case recorded: Seq[_] =>
        //println("recorded:\n  " + recorded.mkString("\n  "))
        (recorded zip expectations) map { re =>
          if (!re._2.isDefinedAt(re._1))
            throw new Exception("expectation failed for: " + re._1)
        }
        if (recorded.size != expectations.size)
          throw new Exception("expectations size doesn't match recorded size")
      case whatever =>
        throw new Exception("Did not expect: " + whatever)
    }

    system.stop(actor)
    system.shutdown()
    system.awaitTermination()
  }

  class TestSbtDeathActor extends Actor {

    val pool = context.actorOf(Props(new ChildPool(new MockSbtProcessFactory)), "pool")

    var theSbt: Option[ActorRef] = None
    var replyTo: Option[ActorRef] = None
    var recording = Vector.empty[Any]

    def record(x: Any): Unit = {
      //println("recording: " + x)
      recording :+= x
    }

    def waitingForNoReservations: Receive = {
      case changed: SbtReservationsChanged if changed.reservations.isEmpty =>
        record(changed)
        for (r <- replyTo)
          r ! recording

      case whatever =>
        record(whatever)
    }

    def waitingForSbtReply: Receive = {
      case response: protocol.NameResponse =>
        record(response)
        // kill our reserved sbt
        theSbt.foreach({ sbt => sbt ! PoisonPill })
        theSbt = None
        context.become(waitingForNoReservations)

      case whatever =>
        record(whatever)
    }

    def waitingForGrant: Receive = {
      case grant: SbtGranted =>
        record(grant)

        // ask sbt for something
        grant.reservation.sbt.foreach({ sbt =>
          theSbt = Some(sbt)
          sbt ! protocol.NameRequest(sendEvents = false)
        })

        context.become(waitingForSbtReply)

      case whatever =>
        record(whatever)
    }

    override def receive = {
      case "test" =>
        pool ! SubscribeSbts(self)

        pool ! RequestAnSbt(SbtReservation("1", "Request 1"))

        replyTo = Some(sender)
        context.become(waitingForGrant)
    }
  }

  @Test
  def testSbtDeath(): Unit = {
    val system = ActorSystem("testPoolSystem")

    val actor = system.actorOf(Props(new TestSbtDeathActor), "child-pool-sbt-death-test")

    val expectations = Seq[PartialFunction[Any, Unit]](
      { case SbtReservationsChanged(x) if x.isEmpty => },
      { case SbtGranted(reservation) if reservation.id == "1" => },
      { case SbtReservationsChanged(x) if x.size == 1 => },
      { case response: protocol.NameResponse => },
      { case SbtReservationsChanged(x) if x.isEmpty => })

    implicit val timeout = Timeout(5, TimeUnit.SECONDS)
    Await.result(actor ? "test", timeout.duration) match {
      case recorded: Seq[_] =>
        if (recorded.size != expectations.size)
          throw new Exception("expectations size doesn't match recorded size, recorded: " + recorded.mkString("\n  "))
        (recorded zip expectations) map { re =>
          if (!re._2.isDefinedAt(re._1))
            throw new Exception("expectation failed for: " + re._1)
        }
      case whatever =>
        throw new Exception("Did not expect: " + whatever)
    }

    system.stop(actor)
    system.shutdown()
    system.awaitTermination()
  }
}
