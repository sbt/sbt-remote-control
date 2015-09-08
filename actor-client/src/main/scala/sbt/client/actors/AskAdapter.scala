/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import akka.actor._
import sbt.client.actors.Request
import akka.util.Timeout
import scala.concurrent.{ ExecutionContext, Promise, Future }
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration

class AskAdapter[T](target: ActorRef, req: ActorRef => Request[T], result: Promise[T], timeout: Timeout) extends Actor {
  import AskAdapter._

  def receive: Receive = {
    val r = req(self)
    val matchResponse = r.MatchResponse
    target ! r
    context.setReceiveTimeout(timeout.duration)

    {
      case msg =>
        context.setReceiveTimeout(Duration.Undefined)
        msg match {
          case `matchResponse`(out) =>
            result.success(out)
          case ReceiveTimeout =>
            result.failure(new TimeoutException(s"Request timed out after $timeout"))
          case m =>
            result.failure(new InvalidResultException(s"Got invalis response from target: $m"))
        }
        context stop self
    }
  }
}

object AskAdapter {
  class InvalidResultException(msg: String) extends Exception(msg)

  def props[T](target: ActorRef, req: ActorRef => Request[T], result: Promise[T], timeout: Timeout): Props =
    Props(new AskAdapter[T](target, req, result, timeout))

}

trait WithAskAdapter { this: Actor =>
  def asAsk[T](target: ActorRef)(req: ActorRef => Request[T])(implicit timeout: Timeout): Future[T] = {
    val p = Promise[T]()
    context.actorOf(AskAdapter.props(target, req, p, timeout))
    p.future
  }

}