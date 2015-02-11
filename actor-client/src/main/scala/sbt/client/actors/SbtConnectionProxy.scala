/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import akka.actor._
import java.io.File
import sbt.client.{ Subscription, SbtConnector, SbtClient, SbtChannel }
import scala.concurrent.ExecutionContext
import sbt.serialization._

trait Request[Resp] {
  def sendTo: ActorRef
  final def response(in: Resp)(implicit sender: ActorRef): Unit = sendTo.tell(in, sender)
}

object SbtClientBuilder {
  import SbtConnectionProxy._
  case class Subscription(subscription: sbt.client.Subscription)
  case class Client(client: SbtClient)
  case class Error(reconnect: Boolean, error: String)
  case object Close

  def props(request: SbtConnectionProxy.NewClient,
    sbtClientProxyProps: SbtClient => Props): Props = Props(new SbtClientBuilder(request, sbtClientProxyProps))
}

final class SbtClientBuilder(request: SbtConnectionProxy.NewClient,
  sbtClientProxyProps: SbtClient => Props) extends Actor with ActorLogging {
  import SbtClientBuilder._

  private var subscription: sbt.client.Subscription = null

  private def connected(client: SbtClient, sbtClientProxy: ActorRef): Receive = {
    case Client(c) =>
      sbtClientProxy ! SbtClientProxy.UpdateClient(c)
      context.become(connected(c, sbtClientProxy))
    case Error(true, error) =>
      log.warning(s"Client ${client.configName}-${client.humanReadableName}-${client.uuid} experienced an error[will try reconnect]: $error")
    case e @ Error(false, error) =>
      log.error(s"Client ${client.configName}-${client.humanReadableName}-${client.uuid} experienced an error[CANNOT RECONNECT]: $error")
      sbtClientProxy ! SbtClientProxy.Close(self)
    case Close =>
      sbtClientProxy ! SbtClientProxy.Close(self)
    case Terminated(a) =>
      context.unwatch(a)
      context stop self
    case SbtClientProxy.Closed => // don't care
  }

  private def process(subscription: Option[sbt.client.Subscription], client: Option[SbtClient], error: Option[Error]): Unit = {
    (subscription, client, error) match {
      case (maybeSubscription, maybeClient, Some(e @ Error(false, _))) =>
        log.error(s"Fatal error, failed to connect: $e")
        request.responseWithError(false, e.error)
        maybeSubscription.foreach(_.cancel())
        maybeClient.foreach(_.close())
        context stop self
      case (Some(s), Some(c), _) =>
        log.debug("Connected")
        this.subscription = s
        val clientActor = context.actorOf(sbtClientProxyProps(c))
        context.watch(clientActor)
        request.responseWithClient(clientActor)
        context.become(connected(c, clientActor))
      case (_, _, _) => context.become(run(subscription, client, error))
    }
  }

  private def run(subscription: Option[sbt.client.Subscription], client: Option[SbtClient], error: Option[Error]): Receive = {
    case Subscription(s) => process(Some(s), client, error)
    case Client(c) => process(subscription, Some(c), error)
    case e: Error => process(subscription, client, Some(e))
  }

  def receive: Receive = run(None, None, None)

  override def postStop(): Unit = {
    super.postStop()
    if (subscription != null) {
      subscription.cancel()
      subscription = null
    }
  }
}

object SbtConnectionProxy {
  sealed trait Notification
  object Notifications {
    case object BuilderAwaitingChannel extends Notification
  }

  case class Connected(client: SbtClient)
  case class ClientSubscription(subscription: Subscription, client: ActorRef, builder: ActorRef)
  case object Closing

  sealed trait Response
  sealed trait NewClientResponse extends Response
  object NewClientResponse {
    case class Connected(client: ActorRef) extends NewClientResponse
    case class Error(reconnect: Boolean, error: String) extends NewClientResponse
  }
  case object Closed extends Response

  sealed trait LocalRequest[Resp] extends Request[Resp]
  case class NewClient(sendTo: ActorRef) extends LocalRequest[NewClientResponse] {
    def responseWithClient(client: ActorRef)(implicit sender: ActorRef): Unit =
      response(NewClientResponse.Connected(client))
    def responseWithError(reconnect: Boolean, error: String)(implicit sender: ActorRef): Unit =
      response(NewClientResponse.Error(reconnect, error))
  }
  case class Close(sendTo: ActorRef) extends LocalRequest[Closed.type] {
    def closed()(implicit sender: ActorRef): Unit = response(Closed)
  }

  def props(connector: SbtConnector,
    createClient: SbtChannel => SbtClient = SbtClient.apply,
    builderProps: (SbtConnectionProxy.NewClient, SbtClient => Props) => Props = (nc, cb) => SbtClientBuilder.props(nc, cb),
    clientProps: SbtClient => Props = c => SbtClientProxy.props(c)(scala.concurrent.ExecutionContext.Implicits.global),
    notificationSink: SbtConnectionProxy.Notification => Unit = _ => ())(implicit ex: ExecutionContext): Props =
    Props(new SbtConnectionProxy(connector = connector,
      createClient = createClient,
      builderProps = builderProps,
      clientProps = clientProps,
      ec = ex,
      notificationSink = notificationSink))
}

final class SbtConnectionProxy(connector: SbtConnector,
  createClient: SbtChannel => SbtClient,
  builderProps: (SbtConnectionProxy.NewClient, SbtClient => Props) => Props,
  clientProps: SbtClient => Props,
  ec: ExecutionContext,
  notificationSink: SbtConnectionProxy.Notification => Unit = _ => ()) extends Actor with ActorLogging {
  import SbtConnectionProxy._

  private def onConnect(builder: ActorRef)(client: SbtClient): Unit = {
    builder ! SbtClientBuilder.Client(client)
  }

  private def onError(builder: ActorRef)(reconnect: Boolean, error: String): Unit = {
    builder ! SbtClientBuilder.Error(reconnect, error)
  }

  private def onRequest(req: LocalRequest[_]): Unit = req match {
    case r: NewClient =>
      val builder = context.actorOf(builderProps(r, clientProps))
      context.watch(builder)
      val subs = connector.openChannel(channel => onConnect(builder)(createClient(channel)),
        onError(builder))(ec)
      builder ! SbtClientBuilder.Subscription(subs)
      notificationSink(Notifications.BuilderAwaitingChannel)
    case r: Close =>
      context.children.foreach(_ ! SbtClientBuilder.Close)
      context.become(closing(r))
  }

  private def closing(request: Close): Receive = {
    if (context.children.isEmpty) {
      connector.close()
      request.closed()
      context stop self
    }

    {
      case Terminated(a) =>
        context.become(closing(request))
      case req: LocalRequest[_] =>
        log.warning(s"Received request $req while closing")
        req.sendTo ! Closing
      case msg =>
        log.warning(s"Received $msg while closing")
        sender ! Closing
    }
  }

  private def running(): Receive = {
    case req: LocalRequest[_] => onRequest(req)
    case Terminated(_) => // don't care in this state
  }

  def receive: Receive = {
    running()
  }
}