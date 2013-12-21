package com.typesafe.sbtrc
package server

import sbt.{
  ScopedKey,
  State
}


case class KeyValueClientListener[T](
  key: ScopedKey[T],
  client: SbtClient
) {
  /** Disconnect a client from this listener. */
  def disconnect(c: SbtClient): KeyValueClientListener[T] =
    copy(client = client without c)
}

/** Represents the current state of the sbt server we use to drive 
 * events/handle client requests.
 */
case class SbtServerState(
  buildState: State,  
  eventListeners: SbtClient = NullSbtClient,
  buildListeners: SbtClient = NullSbtClient,
  keyListeners: Seq[KeyValueClientListener[_]] = Seq.empty
) {
  
  
  /** Remove a client from any registered listeners. */
  def disconnect(client: SbtClient): SbtServerState =
    copy(
        eventListeners = eventListeners without client,
        buildListeners = buildListeners without client,
        keyListeners = keyListeners map (_ disconnect client)
    )
}

