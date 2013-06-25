package com.typesafe.sbtrc

import java.util.concurrent.ThreadFactory
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

class NamedThreadFactory(name: String) extends ThreadFactory {
  private val num = new AtomicInteger(1)
  override def newThread(r: Runnable): Thread = {
    val t = Executors.defaultThreadFactory().newThread(r)
    t.setName(name + "-" + num.getAndIncrement())
    t
  }
}

object NamedThreadFactory {
  def newPool(name: String) = Executors.newCachedThreadPool(new NamedThreadFactory(name))
}
