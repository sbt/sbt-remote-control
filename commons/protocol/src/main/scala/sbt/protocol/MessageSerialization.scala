package sbt.protocol

import sbt.serialization._

/** The implementation of Message serialization */
private[protocol] object MessageSerialization {
  // this makes it prettier when writing json by hand e.g. in JavaScript
  private def removeDollar(s: String) = {
    val i = s.lastIndexOf('$')
    if (i >= 0)
      s.substring(0, i)
    else
      s
  }
  // avoiding class.getSimpleName because apparently it's buggy with some
  // Scala name manglings
  private def lastChunk(s: String) = {
    val i = s.lastIndexOf('.')
    if (i >= 0)
      s.substring(i + 1)
    else
      s
  }
  private[protocol] def makeSimpleName(klass: Class[_]): String =
    removeDollar(lastChunk(klass.getName))
  /*
  private def msg[T <: Message](implicit f: ReadsWrites[T], mf: ClassManifest[T]): (Class[T], (String, Reads[T], Writes[T])) =
    mf.runtimeClass.asInstanceOf[Class[T]] -> (makeSimpleName(mf.runtimeClass), f, f)

  private val messages: Map[Class[_], (String, Reads[_], Writes[_])] = Map(
    msg[TaskLogEvent],
    msg[CoreLogEvent],
    msg[BackgroundJobLogEvent],
    msg[KillServerRequest],
    msg[CancelExecutionRequest],
    msg[CancelExecutionResponse],
    msg[RegisterClientRequest],
    msg[ReadLineRequest],
    msg[ReadLineResponse],
    msg[ConfirmRequest],
    msg[ConfirmResponse],
    msg[ReceivedResponse],
    msg[ExecutionRequestReceived],
    msg[ExecutionRequest],
    msg[KeyExecutionRequest],
    msg[ExecutionStarting],
    msg[ExecutionWaiting],
    msg[ExecutionFailure],
    msg[ExecutionSuccess],
    msg[ListenToEvents],
    msg[UnlistenToEvents],
    msg[ListenToBuildChange],
    msg[UnlistenToBuildChange],
    msg[SendSyntheticBuildChanged],
    msg[ListenToValue],
    msg[UnlistenToValue],
    msg[SendSyntheticValueChanged],
    msg[KeyNotFound],
    msg[BuildStructureChanged],
    msg[ValueChanged],
    msg[ErrorResponse],
    msg[TaskStarted],
    msg[TaskFinished],
    msg[CommandCompletionsRequest],
    msg[CommandCompletionsResponse],
    msg[KeyLookupResponse],
    msg[KeyLookupRequest],
    msg[AnalyzeExecutionRequest],
    msg[AnalyzeExecutionResponse],
    msg[TaskEvent],
    msg[BuildLoaded],
    msg[BuildFailedToLoad],
    msg[BackgroundJobStarted],
    msg[BackgroundJobFinished],
    msg[BackgroundJobEvent])

  private val readsIndex: Map[String, Reads[_]] =
    (for {
      (_, (name, reads, _)) <- messages
    } yield name -> reads).toMap
*/

  val messagePickler: SbtPickler[Message] = ???

  val messageUnpickler: SbtUnpickler[Message] = ???
}
