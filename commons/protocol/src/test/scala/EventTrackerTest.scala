/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import sbt.protocol._
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

class EventTrackerTest {

  @Test
  def testEventTracker(): Unit = {
    val key1 = AttributeKey("name", TypeInfo("java.lang.String"))
    val key2 = AttributeKey("foo", TypeInfo("java.lang.Integer"))
    val build = new java.net.URI("file:///test/project")
    val scope1 = SbtScope(project = Some(
      ProjectReference(build, "test1")))
    val scope2 = SbtScope(project = Some(
      ProjectReference(build, "test2")))
    val scopedKey1 = ScopedKey(key1, scope1)
    val scopedKey2 = ScopedKey(key2, scope2)
    val clientInfo = ClientInfo(java.util.UUID.randomUUID.toString(), "clientyclient", "Client Test")
    val creationEvents = List(ExecutionWaiting(1, "frobulate", clientInfo),
      ExecutionStarting(1),
      TaskStarted(1, 1, Some(scopedKey1)),
      TaskStarted(1, 2, Some(scopedKey2)),
      TaskFinished(1, 1, key = Some(scopedKey1), success = false),
      ExecutionWaiting(2, "foobar", clientInfo),
      ExecutionWaiting(3, "barfoo", clientInfo),
      ExecutionWaiting(4, "goesaway", clientInfo),
      ExecutionStarting(4),
      ExecutionSuccess(4))

    val engine = ImpliedState.processEvents(ImpliedState.ExecutionEngine.empty, creationEvents)
    assertEquals(2, engine.waiting.size)
    assertEquals(1, engine.started.size)
    assertEquals(1, engine.started.values.head.tasks.size)
    assertEquals("frobulate", engine.started.values.head.command)

    val toReach = ImpliedState.eventsToReachEngineState(engine)
    val recreated = ImpliedState.processEvents(ImpliedState.ExecutionEngine.empty, toReach)
    assertEquals(engine, recreated)

    val toEmpty = ImpliedState.eventsToEmptyEngineState(engine, success = false)
    val emptied = ImpliedState.processEvents(engine, toEmpty)
    assertEquals(0, emptied.waiting.size)
    assertEquals(0, emptied.started.size)
  }
}
