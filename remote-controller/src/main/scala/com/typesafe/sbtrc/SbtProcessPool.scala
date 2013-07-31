package com.typesafe.sbtrc

import akka.actor._
import java.io.File
import properties.SbtRcProperties
import com.typesafe.sbtrc.io.ShimWriter
import com.typesafe.sbtrc.launching.SbtProcessLauncher

// the "id" can be anything as long as you ensure it's unique
// for the pool; use a uuid if in doubt. taskName is a human-readable
// description of what you will do with the reservation.
// "owner" will default to the sending actor, specify it
// if that is wrong. "sbt" will be filled in when the reservation
// is granted.
case class SbtReservation(id: String, taskName: String, owner: Option[ActorRef] = None, sbt: Option[ActorRef] = None)

sealed trait SbtProcessPoolRequest

case class SubscribeSbts(ref: ActorRef) extends SbtProcessPoolRequest
case class UnsubscribeSbts(ref: ActorRef) extends SbtProcessPoolRequest
case class RequestAnSbt(reservation: SbtReservation) extends SbtProcessPoolRequest
case class ReleaseAnSbt(reservationId: String) extends SbtProcessPoolRequest
case class ForceStopAnSbt(reservationId: String) extends SbtProcessPoolRequest

// sent to owner of SbtReservation only.
// Once granted, the owner can use the sbt actor until 1) the owner sends ReleaseAnSbt
// or 2) the sbt actor dies. An sbt actor cannot be "taken away" because
// that would create all sorts of annoying cases to handle.
// The owner should watch for death of the sbt actor.
case class SbtGranted(reservation: SbtReservation)

sealed trait SbtProcessPoolEvent

// This event is intended for debugging or user feedback
case class SbtReservationsChanged(reservations: Set[SbtReservation]) extends SbtProcessPoolEvent

// this is to allow test mocks
trait SbtProcessFactory {
  def init(log: akka.event.LoggingAdapter): Unit = {}
  def newChild(actorFactory: ActorRefFactory): ActorRef
}

class DefaultSbtProcessFactory(val workingDir: File, val sbtProcessLauncher: SbtProcessLauncher) extends SbtProcessFactory {
  override def init(log: akka.event.LoggingAdapter): Unit = {
    // TODO - We should error out on bad version numbers!
    val sbtBinaryVersion =
      io.SbtVersionUtil.findProjectBinarySbtVersion(workingDir).getOrElse("0.12")
    for (writer <- ShimWriter.knownShims(SbtRcProperties.APP_VERSION, sbtBinaryVersion)) {
      // we update shim plugin files ONLY if they exist, because when they
      // are missing it may be because they aren't applicable. Only the
      // remote probe can decide to ADD a shim file.
      val updated =
        if (ShimWriter.alwaysIncludedShims contains writer.name) {
          writer.ensureExists(workingDir)
        } else writer.updateIfExists(workingDir)
      if (updated)
        log.info(s"Updated shim plugin for ${writer.name}")

    }
  }

  override def newChild(actorFactory: ActorRefFactory): ActorRef = SbtProcess(actorFactory, workingDir, sbtProcessLauncher)
}

// This class manages a pool of sbt processes.
// To use sbt, you reserve an sbt instance, and then
// communicate with that instance. Of course you have to
// be prepared to wait to get the instance.
class ChildPool(val childFactory: SbtProcessFactory, val minChildren: Int = 1, val maxChildren: Int = 3)
  extends Actor with EventSourceActor with ActorLogging {

  var reserved = Set.empty[SbtReservation]
  var available = Set.empty[ActorRef]
  var waiting = Seq.empty[SbtReservation]

  def total = reserved.size + available.size

  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  def emitChangedAfter[R](body: => R): R = {
    // "reserved" is supposed to be lazy-evaluated so after the body runs
    emitEventIfOutermost({ SbtReservationsChanged(reserved) })(body)
  }

  def release(id: String): Unit = {
    for (r <- reserved.find(_.id == id)) {
      emitChangedAfter {
        reserved = reserved - r
        r.owner.foreach(unwatch)
        r.sbt.foreach({ sbt =>
          // we may have removed the reservation due to sbt crash,
          // if so we don't put that sbt back in the pool obvs
          if (sbt.isTerminated) {
            log.debug("sbt child is dead, not returning to pool {}", sbt)
          } else {
            available = available + sbt
            log.debug("returned to sbt pool {}", sbt)
          }
          // we may need to start a replacement sbt, or use the newly-available one
          check()
        })
      }
    }
  }

  def forceStop(id: String): Unit = {
    for (r <- reserved.find(_.id == id)) {
      // simply killing the sbt process should eventually make
      // ProcessActor die which should make the sbt actor die
      // which should send us Terminated which should cause us
      // to drop the reservation. whew!
      r.sbt.foreach { sbt =>
        log.debug("Killing sbt process {} due to forceStop request on {}", sbt, id)
        sbt ! KillSbtProcess
      }
    }

    // if we stop and haven't gotten a reservation yet,
    // we don't want a reservation
    waiting = waiting.filter(_.id != id)
  }

  def startNewChild(): Unit = {
    require(total < maxChildren)
    val child = childFactory.newChild(context)
    log.debug("started new sbt child {}; {} were in use, {} were idle", child, reserved.size, available.size)
    available = available + child
    watch(child)
  }

  def grantReservation(): Unit = emitChangedAfter {
    require(waiting.nonEmpty)
    require(available.nonEmpty)
    val child = available.head
    available = available - child
    val grantee = waiting.head
    waiting = waiting.tail
    val newReservation = grantee.copy(sbt = Some(child))
    reserved = reserved + newReservation
    grantee.owner.foreach({ o =>
      watch(o)
      o ! SbtGranted(newReservation)
    })
  }

  def check(): Unit = {
    if (waiting.nonEmpty) {
      if (available.isEmpty && reserved.size < maxChildren)
        startNewChild()
      if (available.nonEmpty)
        grantReservation()
    }
  }

  def queue(reservation: SbtReservation): Unit = {
    require(reservation.owner.isDefined)
    reservation.owner.foreach(watch)
    waiting = waiting :+ reservation
    check()
  }

  override def receive = {
    case req: SbtProcessPoolRequest => req match {

      case RequestAnSbt(reservationMayNotHaveOwner) =>
        require(reservationMayNotHaveOwner.sbt.isEmpty)
        val reservation =
          reservationMayNotHaveOwner.copy(owner =
            Some(reservationMayNotHaveOwner.owner.getOrElse(sender)))
        queue(reservation)

      case ReleaseAnSbt(id) =>
        release(id)

      case ForceStopAnSbt(id) =>
        forceStop(id)

      case SubscribeSbts(ref) =>
        ref ! SbtReservationsChanged(reserved) // send current state
        subscribe(ref)

      case UnsubscribeSbts(ref) =>
        unsubscribe(ref)
    }

    case Terminated(ref) =>
      onTerminated(ref)
  }

  override def onTerminated(ref: ActorRef): Unit = {
    // chain up to handle event listeners
    super.onTerminated(ref)

    // handle terminated sbt child by dropping its reservation
    reserved.find(_.sbt == Some(ref)).foreach { r =>
      log.debug("sbt child terminated, dropping reservation {}", r)
      release(r.id)
    }

    // handle terminated reservation owner
    reserved.find(_.owner == Some(ref)).foreach { r =>
      log.debug("owner terminated, dropping reservation {}", r)
      release(r.id)
    }

    // handle terminated sbt child which was idle
    if (available contains ref) {
      available = available - ref
    }
  }

  override def preStart() = {
    log.debug("preStart")
    childFactory.init(log)
  }

  override def postStop() = {
    log.debug("postStop")
  }
}
