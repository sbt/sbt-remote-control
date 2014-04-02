package sbt
package client

trait Subscription {
  def cancel(): Unit
}