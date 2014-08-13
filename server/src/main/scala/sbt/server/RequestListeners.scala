package sbt
package server

/**
 * Represents the current listeners at the time a request is
 * handed off from ReadOnlyServerEngine to ServerEngine.
 */
trait RequestListeners {
  def buildListeners: SbtClient
  def keyListeners: Seq[KeyValueClientListener[_]]
}

