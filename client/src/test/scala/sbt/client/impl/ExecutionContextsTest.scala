/**
 *   Copyright (C) 2014 Typesafe Inc. <http://typesafe.com>
 */
package sbt.client.impl

import org.junit.Assert._
import org.junit._

class ExecutionContextsTest {

  @Test
  def testSerializedCached(): Unit = {
    val ctx = ExecutionContexts.serializedCachedExecutionContext
    def execute(f: => Unit): Unit = {
      ctx.execute(new Runnable { override def run() = f })
    }
    // supposed to run one thing at a time, in order.
    @volatile var items = Vector.empty[Int]
    for (v <- 1 to 100)
      execute(items :+= v)
    val latch = new java.util.concurrent.CountDownLatch(1)
    execute(latch.countDown())
    latch.await()
    assertEquals(100, items.size)
    for (v <- 1 to 100)
      assertEquals(v, items(v - 1))
  }
}
