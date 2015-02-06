/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
package sbt.protocol

import org.junit.Assert._
import org.junit._
import sbt.protocol._
import sbt.serialization.TypeExpression
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

class EventTrackerTest {

  @Test
  def testEventTracker(): Unit = {
    val build = new java.net.URI("file:///test/project")
    val projectRef1 = ProjectReference(build, "test1")
    val projectRef2 = ProjectReference(build, "test2")
    val key1 = AttributeKey("name", TypeExpression("java.lang.String", Nil))
    val key2 = AttributeKey("foo", TypeExpression("java.lang.Integer", Nil))
    val scope1 = SbtScope(project = Some(projectRef1))
    val scope2 = SbtScope(project = Some(projectRef2))
    val scopedKey1 = ScopedKey(key1, scope1)
    val scopedKey2 = ScopedKey(key2, scope2)
    val clientInfo = ClientInfo(java.util.UUID.randomUUID.toString(), "clientyclient", "Client Test",
      ProtocolVersion1, Vector.empty)
    val creationEvents = List(ExecutionWaiting(1, "frobulate", clientInfo),
      ExecutionStarting(1),
      TaskStarted(1, 1, Some(scopedKey1)),
      TaskStarted(1, 2, Some(scopedKey2)),
      TaskFinished(1, 1, key = Some(scopedKey1), success = false, message = Some("Task error message")),
      BackgroundJobStarted(1, BackgroundJobInfo(id = 3, spawningTask = scopedKey2, humanReadableName = "Some job 1")),
      BackgroundJobStarted(1, BackgroundJobInfo(id = 4, spawningTask = scopedKey2, humanReadableName = "Some job 2")),
      ExecutionWaiting(2, "foobar", clientInfo),
      ExecutionWaiting(3, "barfoo", clientInfo),
      ExecutionWaiting(4, "goesaway", clientInfo),
      ExecutionStarting(4),
      ExecutionSuccess(4),
      BackgroundJobFinished(1, 4))

    val engine = ImpliedState.processEvents(ImpliedState.ExecutionEngine.empty, creationEvents)
    assertEquals(2, engine.waiting.size)
    assertEquals(1, engine.started.size)
    assertEquals(1, engine.started.values.head.tasks.size)
    assertEquals(1, engine.jobs.size)
    assertEquals("frobulate", engine.started.values.head.command)

    val toReach = ImpliedState.eventsToReachEngineState(engine).map(_.event)
    val recreated = ImpliedState.processEvents(ImpliedState.ExecutionEngine.empty, toReach)
    assertEquals(engine, recreated)

    val toEmpty = ImpliedState.eventsToEmptyEngineState(engine, success = false).map(_.event)
    val emptied = ImpliedState.processEvents(engine, toEmpty)
    assertEquals(0, emptied.waiting.size)
    assertEquals(0, emptied.started.size)
    assertEquals(0, emptied.jobs.size)
  }
}
