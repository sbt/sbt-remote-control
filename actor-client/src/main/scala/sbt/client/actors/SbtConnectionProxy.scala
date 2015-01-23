/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import akka.actor._
import java.io.File
import sbt.client.{ Subscription, SbtConnector, SbtClient, SbtChannel }
import scala.concurrent.ExecutionContext
import sbt.protocol.CoreProtocol._

trait Request[Resp] {
  def sendTo: ActorRef
  final def response(in: Resp)(implicit sender: ActorRef): Unit = sendTo.tell(in, sender)
}

object SbtClientBuilder {
  import SbtConnectionProxy._
  case class Subscription(subscription: sbt.client.Subscription)
  case class Client(client: SbtClient)
  case class Error(reconnect: Boolean, error: String)
  sealed trait Result
  object Result {
    case class Success(request: NewClient, client: SbtClient, subscription: sbt.client.Subscription, builder: ActorRef) extends Result
    case class Failure(request: NewClient, subscription: sbt.client.Subscription, error: Error) extends Result
  }
  case class Reconnect(subscription: sbt.client.Subscription, client: SbtClient, builder: ActorRef)
  case class Disconnect(subscription: sbt.client.Subscription, builder: ActorRef)

  def props(request: SbtConnectionProxy.NewClient, notificationSink: ActorRef): Props = Props(new SbtClientBuilder(request, notificationSink))
}

final class SbtClientBuilder(request: SbtConnectionProxy.NewClient, notificationSink: ActorRef) extends Actor with ActorLogging {
  import SbtClientBuilder._

  private def connected(subscription: sbt.client.Subscription, client: SbtClient): Receive = {
    case Client(c) =>
      notificationSink ! Reconnect(subscription, c, self)
      context.become(connected(subscription, c))
    case Error(true, error) =>
      log.warning(s"Client ${client.configName}-${client.humanReadableName}-${client.uuid} experienced an error[will try reconnect]: $error")
    case e @ Error(false, error) =>
      log.error(s"Client ${client.configName}-${client.humanReadableName}-${client.uuid} experienced an error[CANNOT RECONNECT]: $error")
      notificationSink ! Disconnect(subscription, self)
      context stop self
  }

  private def process(subscription: Option[sbt.client.Subscription], client: Option[SbtClient], error: Option[Error]): Unit = {
    (subscription, client, error) match {
      case (Some(s), Some(c), _) =>
        notificationSink ! Result.Success(request, c, s, self)
        context.become(connected(s, c))
      case (Some(s), _, Some(e @ Error(false, _))) =>
        notificationSink ! Result.Failure(request, s, e)
        context stop self
      case (_, _, _) => context.become(run(subscription, client, error))
    }
  }

  private def run(subscription: Option[sbt.client.Subscription], client: Option[SbtClient], error: Option[Error]): Receive = {
    case Subscription(s) => process(Some(s), client, error)
    case Client(c) => process(subscription, Some(c), error)
    case e: Error => process(subscription, client, Some(e))
  }

  def receive: Receive = run(None, None, None)
}

object SbtConnectionProxy {
  sealed trait Notification
  object Notifications {
    case object BuilderAwaitingChannel extends Notification
    case object CleanedUpAfterClientClosure extends Notification
    case object ClientClosedDueToDisconnect extends Notification
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

  case class State(clients: Map[ActorRef, ClientSubscription] = Map.empty[ActorRef, ClientSubscription])

  def props(connector: SbtConnector,
    createClient: SbtChannel => SbtClient = SbtClient.apply,
    builderProps: (SbtConnectionProxy.NewClient, ActorRef) => Props = (nc, ns) => SbtClientBuilder.props(nc, ns),
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
  builderProps: (SbtConnectionProxy.NewClient, ActorRef) => Props,
  clientProps: SbtClient => Props,
  ec: ExecutionContext,
  notificationSink: SbtConnectionProxy.Notification => Unit = _ => ()) extends Actor with ActorLogging {
  import SbtConnectionProxy._

  private def onConnect(builder: ActorRef)(client: SbtClient): Unit = {
    log.debug("Connected")
    builder ! SbtClientBuilder.Client(client)
  }

  private def onError(builder: ActorRef)(reconnect: Boolean, error: String): Unit = {
    builder ! SbtClientBuilder.Error(reconnect, error)
  }

  private def onRequest(req: LocalRequest[_], state: State): Unit = req match {
    case r: NewClient =>
      val builder = context.actorOf(builderProps(r, self))
      context.watch(builder)
      val subs = connector.openChannel(channel => onConnect(builder)(createClient(channel)),
        onError(builder))(ec)
      builder ! SbtClientBuilder.Subscription(subs)
      notificationSink(Notifications.BuilderAwaitingChannel)
    case r: Close =>
      state.clients.foreach {
        case (_, c) =>
          c.subscription.cancel()
          context.unwatch(c.client)
          context.unwatch(c.builder)
          c.client ! SbtClientProxy.Close(self)
          context stop c.builder
      }
      context.become(closing(r, state.clients.values.map(_.client).toSet))
  }

  private def closing(request: Close, awaiting: Set[ActorRef]): Receive = {
    if (awaiting.isEmpty) {
      connector.close()
      request.closed()
      context stop self
    }

    {
      case SbtClientProxy.Closed =>
        context.become(closing(request, awaiting - sender))
      case req: LocalRequest[_] =>
        log.warning(s"Received request $req while closing")
        req.sendTo ! Closing
      case msg =>
        log.warning(s"Received $msg while closing")
        sender ! Closing
    }
  }

  private def running(state: State): Receive = {
    case Terminated(a) => // builder or client can terminate
      context.unwatch(a)
      state.clients.get(a) match {
        case Some(cs) => // Builder terminated
          context.unwatch(cs.client)
          cs.client ! SbtClientProxy.Close(self)
          cs.subscription.cancel()
          context.become(running(state.copy(clients = state.clients - a)))
        case None => // Possible client terminated
          val toBeClosed = state.clients.filter { case (_, cs) => cs.client == a }
          toBeClosed.values.foreach { x =>
            context.unwatch(x.builder)
            x.subscription.cancel()
            context stop x.builder
          }
          if (toBeClosed.size > 0) {
            log.debug(s"Removed ${toBeClosed.size} builders after client closed")
            notificationSink(Notifications.CleanedUpAfterClientClosure)
            context.become(running(state.copy(clients = state.clients -- toBeClosed.keys)))
          }
      }
    case req: LocalRequest[_] => onRequest(req, state)
    case SbtClientBuilder.Result.Success(request, client, subscription, builder) =>
      val clientActor = context.actorOf(clientProps(client))
      context.watch(clientActor)
      request.responseWithClient(clientActor)
      context.become(running(state.copy(clients = state.clients + (builder -> ClientSubscription(subscription, clientActor, builder)))))
    case SbtClientBuilder.Result.Failure(request, subscription, error) =>
      request.responseWithError(error.reconnect, error.error)
      subscription.cancel()
    case SbtClientBuilder.Reconnect(subscription, client, builder) =>
      state.clients.get(builder) match {
        case Some(s) =>
          s.client ! SbtClientProxy.UpdateClient(client)
        case None =>
          log.warning(s"Got Reconnect message from builder $builder, but no corresponding client")
          subscription.cancel()
          context.stop(builder)
      }
    case SbtClientProxy.Closed => // client closed, see if there is a builder we need to stop
      context.unwatch(sender)
      val toBeClosed = state.clients.filter { case (_, cs) => cs.client == sender }
      toBeClosed.values.foreach { x =>
        x.subscription.cancel()
        context.unwatch(x.builder)
        context stop x.builder
      }
      if (toBeClosed.size > 0) {
        log.debug(s"Removed ${toBeClosed.size} builders after client closed")
        notificationSink(Notifications.CleanedUpAfterClientClosure)
        context.become(running(state.copy(clients = state.clients -- toBeClosed.keys)))
      }
    case SbtClientBuilder.Disconnect(subscription, builder) =>
      context.unwatch(builder)
      state.clients.get(builder) match {
        case Some(s) =>
          context.unwatch(s.client)
          s.client ! SbtClientProxy.Close(self)
          s.subscription.cancel()
          notificationSink(Notifications.ClientClosedDueToDisconnect)
          context.become(running(state.copy(clients = state.clients - builder)))
        case None =>
          log.warning(s"Got Disconnect message from builder $builder, but no corresponding client")
          subscription.cancel()
          context.stop(builder)
      }
  }

  def receive: Receive = {
    running(State())
  }
}