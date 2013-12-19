package com.typesafe.sbtrc
package server

import scala.collection.mutable.ArrayBuffer


/** This class represents a stateful, single-threaded monstrosity that can
 *  handle incoming sbt requests, as long as all requests occur on the same thread.
 */
trait SbtServerEngine {
  
  /** Buffer a client request.  If there are other similar requests (e.g. task execution)
   *  the engine should try to join them into one request.
   */
  def bufferRequest(request: ClientRequest): Unit
  /** The engine should run all buffered requests at this point. */
  def runRequests(): Unit
  /** This should return true as long as the server is running. */
  def isRunning(): Boolean
}


abstract class AbstractSbtServerEngine extends SbtServerEngine {
  
  private val buf = ArrayBuffer.empty[ClientRequest]
  
  protected def runRequest(request: ClientRequest): Unit
  
  // TODO - implement request joining here.
  final def bufferRequest(request: ClientRequest): Unit =
    buf.append(request)
    
  final def runRequests(): Unit = {
    buf foreach runRequest
    buf.clear()
  }
    
}