package sbt.server

import sbt.impl.ipc
import ipc.{ MultiClientServer => IpcServer, HandshakeException }
import java.net.ServerSocket
import java.net.SocketTimeoutException
import sbt.protocol._
import sbt.serialization._
import scala.util.control.NonFatal

/**
 * A class that will spawn up a thread to handle client connection requests.
 *
 * TODO - We should use netty for this rather than spawning so many threads.
 */
class SbtServerSocketHandler(serverSocket: ServerSocket, msgHandler: SocketMessage => Unit,
  serverEngineLogFile: java.io.File) {

  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  private val TIMEOUT_TO_DEATH: Int = 3 * 60 * 1000
  // TODO - This should be configurable.
  private val log = FileLogger(new java.io.File(".sbtserver/connections/master.log"))

  private val clientLock = new AnyRef {}
  private val clients = collection.mutable.ArrayBuffer.empty[SbtClientHandler]

  private val thread = new Thread("sbt-server-socket-handler") {
    // TODO - Check how long we've been running without a client connected
    // and shut down the server if it's been too long.
    final override def run(): Unit = {
      // we know we're only going to process known handshake/registration messages
      // so the default serializations are (more than) enough.
      val serializations = DynamicSerialization.defaultSerializations

      // TODO - Is this the right place to do this?
      serverSocket.setSoTimeout(TIMEOUT_TO_DEATH)
      while (running.get) {
        try {
          log.log(s"Taking next connection to: ${serverSocket.getLocalPort}")
          val socket = serverSocket.accept()
          log.log(s"New client attempting to connect on port: ${socket.getPort}-${socket.getLocalPort}")
          log.log(s"  Address = ${socket.getLocalSocketAddress}")
          val server = new IpcServer(socket)

          val (uuid, register, registerSerial) = Envelope(server.receive()) match {
            case Envelope(serial, replyTo, req: RegisterClientRequest) =>
              def replyAndException(message: String): Exception = {
                server.replyJson[Message](serial, ErrorResponse(message))
                new HandshakeException(message, null, null)
              }
              clientLock.synchronized {
                // Note there is a race here; two clients with same UUID can connect,
                // the UUID isn't in the list for either, then we append both to the
                // list. But we aren't really worried about evil/hostile clients just
                // detecting buggy clients so don't worry about it. Can't happen unless
                // clients are badly broken (hardcoded fixed uuid?) or malicious.
                if (clients.exists(_.uuid == req.info.uuid))
                  throw replyAndException(s"UUID already in use: ${req.info.uuid}")
              }

              if (!req.info.configName.matches("^[-a-zA-Z0-9]+$"))
                throw replyAndException(s"configName '${req.info.configName}' must be non-empty and ASCII alphanumeric only")

              if (req.info.humanReadableName.isEmpty())
                throw replyAndException(s"humanReadableName must not be empty")

              try { (java.util.UUID.fromString(req.info.uuid), req.info, serial) }
              catch {
                case e: IllegalArgumentException =>
                  throw replyAndException(s"Invalid UUID format: '${req.info.uuid}'")
              }
            case Envelope(serial, _, wtf) =>
              server.replyJson(serial, ErrorResponse("First message must be a RegisterClientRequest"))
              throw new HandshakeException(s"First message from client was ${wtf} instead of register request",
                null, null)
          }

          server.replyJson(registerSerial, ReceivedResponse())

          log.log(s"This client on port ${socket.getPort} has uuid ${uuid} configName ${register.configName} humanReadableName ${register.humanReadableName}")

          // TODO - Clear out any client we had before with this same port *OR* take over that client....
          def onClose(): Unit = {
            clientLock.synchronized(clients.remove(clients.indexWhere(_.uuid == uuid)))
            // TODO - See if we can reboot the timeout on the server socket waiting
            // for another connection.
          }
          val client = new SbtClientHandler(uuid, register.configName, register.humanReadableName,
            server, msgHandler, onClose)
          client.send(CoreLogEvent(LogMessage(LogMessage.DEBUG, s"sbt server socket logs are in: ${log.file.getAbsolutePath}")))
          client.send(CoreLogEvent(LogMessage(LogMessage.DEBUG, s"sbt general server logs are in: ${serverEngineLogFile.getAbsolutePath}")))
          clientLock.synchronized {
            clients.append(client)
            log.log(s"Connected Clients: ${clients map (c => s"${c.configName}-${c.uuid}") mkString ", "}")
          }
        } catch {
          case e: HandshakeException =>
            // For now we ignore these, as someone trying to discover if we're listening to ports
            // will connect and close before we can read the handshake.
            // We should formalize what constitutes a catastrophic failure, and make sure we
            // down the server in that instance.
            log.error(s"Handshake exception on socket: ${e.socket.getPort}", e)
          case _: InterruptedException | _: SocketTimeoutException =>
            if (running.get) {
              log.log("Checking to see if clients are empty...")
              // Here we need to check to see if we should shut ourselves down.
              if (clients.isEmpty) {
                log.log("No clients connected after 3 min.  Shutting down.")
                running.set(false)
              } else {
                log.log("We have a client, continuing serving connections.")
              }
            } else {
              log.log(s"socket exception, running=false, exiting")
            }
          case e: java.io.IOException =>
            // this is expected when we close the socket
            log.log(s"Server socket closed ${e.getClass.getName}: ${e.getMessage}")
            running.set(false)
          case e: Throwable =>
            // this one is an unexpected failure
            log.error(s"Unhandled throwable in server socket thread ${e.getClass.getName}: ${e.getMessage}", e)
            running.set(false)
        }
      }
      log.log("Server socket thread exiting.")

      // Cleanup clients, waiting for them to notify their users.
      // We have a complexity that during shutdown, each client will
      // call our close handler above and mutate the array buffer.
      // We do know that nobody will ADD to the array buffer since
      // this thread would do that, but a client COULD self-close before
      // we close it. Clients call our close handler from a different
      // thread from the one we're joining below, to avoid deadlock.
      clientLock.synchronized {
        log.log(s"${clients.size} clients open, will close them...")
        clients.foreach(_.shutdown())
        clients.foreach(_.join())
      }
      // so sometime here after we drop clientLock, "clients" will
      // have all its elements removed from another thread.

      log.log("All client sockets have been closed.")

      // in case we didn't exit due to a stop() call
      try serverSocket.close() catch { case NonFatal(e) => }

      // notify we are closed
      msgHandler(SocketClosed)
    }
  }
  thread.start()

  // Tells the server to stop running.
  def stop(): Unit = {
    running.set(false)
    // be sure we clean up the socket
    try serverSocket.close() catch { case NonFatal(e) => }
  }

  // Blocks the server until we've been told to shutdown by someone.
  def join(): Unit = {
    thread.join()
    log.close()
  }
}
