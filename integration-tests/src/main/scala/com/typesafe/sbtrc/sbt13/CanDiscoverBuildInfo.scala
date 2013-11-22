package com.typesafe.sbtrc
package sbt13

import com.typesafe.sbtrc.protocol._
import com.typesafe.sbtrc.it._
import java.io.File
import akka.actor._
import akka.pattern._
import akka.dispatch._
import concurrent.duration._
import concurrent.Await
import akka.util.Timeout
import akka.pattern.ask

/** Ensures that we can make requests and receive responses from our children. */
class CanDiscoverBuild extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("runTestBuild", "0.13.0")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  object KeyListExtract {
    def unapply(params: Map[String, Any]): Option[KeyList] =
      JsonStructure.unapply[KeyList](params)
  }
  try {
    val classpathResult = concurrent.promise[BuildValue[Seq[sbt.Attributed[File]]]]
    system.actorOf(akka.actor.Props(new akka.actor.Actor {
      child ! TaskKeyRequest(KeyFilter.empty)
      System.err.println("DEBUGME - Issuing TaskKey request.")
      context.setReceiveTimeout(timeout.duration)
      def receive: Receive = {
        case protocol.KeyListResponse(keyList) => // We succeeded!
          System.err.println("DEBUGME - Found task keys - " + keyList.keys.size)
          try {
            val key = keyList.keys.filter { k =>
              (k.key.name == "fullClasspath") // TODO - Get Compile config.
            }.find { k =>
              System.err.println("DEBUGME - Checking scope of " + k + " for compile config")
              k.scope.config == Some("compile") // TODO 
            }.get
            System.err.println("DEBUGME - Using key: " + key)
            child ! TaskValueRequest(key, true)
          } catch {
            case t: Throwable =>
              System.err.println("DEBUGME: Could not find desired key!")
              classpathResult.failure(t)
              context stop self
          }
        case protocol.TaskValueResponse(result) =>
          System.err.println("DEBUGME - Got task value response: " + result)
          classpathResult.complete(
            util.Try {
              System.err.println("DEBUGME - unparsed result = " + result)
              assert(result.isSuccess, "Response was not success! " + result)
              result.asInstanceOf[TaskSuccess[Seq[sbt.Attributed[File]]]].value
            })
        case ReceiveTimeout =>
          classpathResult.failure(new AssertionError("Classpath request timed out."))
      }
    }))

    Await.result(classpathResult.future, timeout.duration).value match {
      case Some(classpath) =>
        // TODO - Verify
        System.err.println("Claspath = " + classpath)
      case None => throw new AssertionError("Unable to determine Classpath of the project.")
    }

    val nameResult = concurrent.promise[BuildValue[String]]
    system.actorOf(akka.actor.Props(new akka.actor.Actor {
      child ! SettingKeyRequest(KeyFilter.empty)
      context.setReceiveTimeout(timeout.duration)
      def receive: Receive = {
        case protocol.KeyListResponse(keyList) => // We succeeded!
          try {
            val key = keyList.keys.find(_.key.name == "name").get
            child ! SettingValueRequest(key)
          } catch {
            case t: Throwable =>
              nameResult.failure(t)
              throw t
          }
        case protocol.SettingValueResponse(value) =>
          nameResult.complete(
            util.Try {
              assert(value.isSuccess, "Response was not success! " + value)
              value.asInstanceOf[TaskSuccess[String]].value
            })
        case ReceiveTimeout =>
          nameResult.failure(new AssertionError("Name request timed out."))
      }
    }))

    Await.result(nameResult.future, timeout.duration).value match {
      case Some(name) => assert(name == "runTestBuild", "Unable to determine name of project, found: " + name)
      case None => throw new AssertionError("Unable to determine name of the project.")
    }

    Await.result(child ? SettingKeyRequest(KeyFilter.empty), timeout.duration) match {
      case protocol.KeyListResponse(keyList) => // We succeeded!
        // TODO - Check the list more thoroughly
        keyList.keys.foreach(println)
        assert(keyList.keys.size > 0, "Key list must not be zero")
        val canFindSourceDirectoryKey =
          keyList.keys.exists { key =>
            (key.key.name == "sourceDirectory") && (key.scope.config == Some("compile"))
          }
        assert(canFindSourceDirectoryKey, "COuld not find `sourceDirectory` in Compile in key list!")
      case whatever => throw new AssertionError("did not get KeyListResponse got " + whatever)
    }
    Await.result(child ? TaskKeyRequest(KeyFilter.empty), timeout.duration) match {
      case protocol.KeyListResponse(keyList) => // We succeeded!
        // TODO - Check the list
        keyList.keys.foreach(println)
        assert(keyList.keys.size > 0, "Key list must not be zero")
        val canFindSourcesKey =
          keyList.keys.exists { key =>
            (key.key.name == "sources") && (key.scope.config == Some("compile"))
          }
        assert(canFindSourcesKey, "COuld not find `sources` in Compile in key list!")
      case whatever => throw new AssertionError("did not get KeyListResponse got " + whatever)
    }
    Await.result(child ? InputTaskKeyRequest(KeyFilter.empty), timeout.duration) match {
      case protocol.KeyListResponse(keyList) => // We succeeded!
        // TODO - Check the list
        keyList.keys.foreach(println)
        assert(keyList.keys.size > 0, "Key list must not be zero")
        val canFindRunKey =
          keyList.keys.exists { key =>
            (key.key.name == "run") && (key.scope.config == Some("compile"))
          }
        assert(canFindRunKey, "Could not find `run in Compile` in key list!")
      case whatever => throw new AssertionError("did not get KeyListResponse got " + whatever)
    }

  } finally {
    system.stop(child)
  }
}