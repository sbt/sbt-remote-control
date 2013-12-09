package com.typesafe.sbtrc
package api

trait Subscription {
  def cancel(): Unit
}