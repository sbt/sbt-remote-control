package sbt

import play.api.libs.json._

package object protocol {
  // TODO - Dangerous-ish
  implicit object IntFormat extends Format[java.lang.Integer] {
    def writes(i: java.lang.Integer): JsValue =
      JsNumber(i.intValue)
    def reads(v: JsValue): JsResult[java.lang.Integer] = v match {
      case JsNumber(value) => JsSuccess(value.toInt)
      case _ => JsError("Unable to convert integer: " + v)
    }
  }
  // Generic Serializers
  implicit object UrlFormat extends Format[java.net.URL] {
    def writes(u: java.net.URL): JsValue =
      JsString(u.toURI.toASCIIString)
    def reads(v: JsValue): JsResult[java.net.URL] =
      JsSuccess(new java.net.URL(v.as[String]))
  }
  implicit object FileFormat extends Format[java.io.File] {
    def writes(u: java.io.File): JsValue =
      JsString(u.toURI.toASCIIString)
    def reads(v: JsValue): JsResult[java.io.File] =
      JsSuccess(new java.io.File(new java.net.URI(v.as[String])))
  }
  implicit object UriFormat extends Format[java.net.URI] {
    def writes(u: java.net.URI): JsValue =
      JsString(u.toASCIIString)
    def reads(v: JsValue): JsResult[java.net.URI] =
      JsSuccess(new java.net.URI(v.as[String]))
  }
  implicit def optionFormat[A](implicit other: Format[A]): Format[Option[A]] =
    new Format[Option[A]] {
      def writes(o: Option[A]): JsValue =
        o match {
          case Some(value) => other.writes(value)
          case None => JsNull // TODO - Is this ok?
        }
      def reads(v: JsValue): JsResult[Option[A]] =
        v match {
          case JsNull =>  JsSuccess(None)
          case value => other.reads(value).map(Some(_))
        }
    }
  implicit def attributedFormat[T](implicit format: Format[T]) =
    new Format[sbt.Attributed[T]] {
      override def writes(t: sbt.Attributed[T]): JsValue =
        JsObject(Seq("attributed" -> JsBoolean(true),
                     "data" -> format.writes(t.data)))
      override def reads(obj: JsValue): JsResult[sbt.Attributed[T]] =
        (obj \ "attributed") match {
          case JsBoolean(true) => 
             format.reads(obj \ "data").map(sbt.Attributed.blank)
          case _ => JsError("not a valid attributed.")
        }
      override def toString = "Format[Attributed["+format+"]"
    }
  implicit object SeverityFormat extends Format[xsbti.Severity] {
    override def writes(in: xsbti.Severity): JsValue =
      JsString(in.toString)
    override def reads(in: JsValue): JsResult[xsbti.Severity] = 
      in match {
        case JsString(s) => JsSuccess(xsbti.Severity.valueOf(s))
        case _ => JsError("Could not find severity: " + in)
      }
  }
  implicit object PositionFormat extends Format[xsbti.Position] {
    override def writes(in: xsbti.Position): JsObject = {
      def defineIf[T](value: xsbti.Maybe[T], name: String)(implicit format: Format[T]): Seq[(String, JsValue)] =
        if(value.isDefined) Seq(name -> format.writes(value.get)) else Nil
      val line = defineIf(in.line, "line")
      val offset = defineIf(in.offset, "offset")
      val pointer = defineIf(in.pointer, "pointer")
      val pointerSpace = defineIf(in.pointerSpace, "pointerSpace")
      val sourcePath = defineIf(in.sourcePath, "sourcePath")
      val sourceFile = defineIf(in.sourceFile, "sourceFile")
      JsObject(Seq("lineContent" -> JsString(in.lineContent)) ++ 
        line ++ 
        offset ++ 
        pointer ++ 
        pointerSpace ++
        sourcePath ++
        sourceFile)
    }
    private def convert[T](o: Option[T]): xsbti.Maybe[T] = 
      o match {
        case Some(value) => xsbti.Maybe.just(value)
        case None => xsbti.Maybe.nothing()
      }
    private class PositionDeserialized(
      override val lineContent: String,
      l: Option[Int],
      o: Option[Int],
      p: Option[Int],
      ps: Option[String],
      sp: Option[String],
      sf: Option[java.io.File]
    ) extends xsbti.Position {
      override def line = convert(l.map(Integer.valueOf))
      override def offset = convert(o.map(Integer.valueOf))
      override def pointer = convert(p.map(Integer.valueOf))
      override def pointerSpace = convert(ps)
      override def sourcePath = convert(sp)
      override def sourceFile = convert(sf)
    }
    override def reads(in: JsValue): JsResult[xsbti.Position] = {
      (in \ "lineContent") match {
        case JsString(lineContent) => 
          val line = (in \ "line").asOpt[Int]
          val offset = (in \ "offset").asOpt[Int]
          val pointer = (in \ "pointer").asOpt[Int]
          val pointerSpace = (in \ "pointerSpace").asOpt[String]
          val sourcePath = (in \ "sourcePath").asOpt[String]
          val sourceFile = (in \ "sourceFile").asOpt[java.io.File]
          JsSuccess(new PositionDeserialized(lineContent.toString, line, offset, pointer, pointerSpace, sourcePath, sourceFile))
        case _ => JsError("Could not deserialize Position")
      }
    }
  }
  // Protocol serializers...  
  implicit val errorResponseFormat = Json.format[ErrorResponse]
    
  // EVENTS
  
  implicit object logEntryFormat extends Format[LogEntry] {
    def writes(entry: LogEntry): JsValue = 
      entry match {
        case LogSuccess(message) =>
          JsObject(Seq(
              "type" -> JsString("success"), 
              "message" -> JsString(message)))
        case LogTrace(klass, message) =>
          JsObject(Seq(
              "type" -> JsString("trace"), 
              "class" -> JsString(klass), 
              "message" -> JsString(message)))
        case LogMessage(level, message) =>
          JsObject(Seq(
              "type" -> JsString("message"), 
              "level" -> JsString(level), 
              "message" -> JsString(message)))
        case LogStdOut(message) =>
          JsObject(Seq(
              "type" -> JsString("stdout"),
              "message" -> JsString(message)))
        case LogStdErr(message) =>
          JsObject(
              Seq("type" -> JsString("stderr"),
              "message" -> JsString(message)))

      }
    // TODO - Nicer error messages.
    def reads(obj: JsValue): JsResult[LogEntry] = {
      (obj \ "type").asOpt[String].collect({
        case "success" => LogSuccess((obj \ "message").as[String])
        case "trace" => LogTrace((obj \ "class").as[String], (obj \ "message").as[String])
        case "message" => LogMessage((obj \ "level").as[String], (obj \ "message").as[String])
        case "stdout" => LogStdOut((obj \ "message").as[String])
        case "stderr" => LogStdErr((obj \ "message").as[String])
      }).map(x => JsSuccess(x)).getOrElse(JsError("Not a log message."))
    }
  }
  implicit val logEventFormat = Json.format[LogEvent]
  private def emptyObjectFormat[A](instance: A) = new Format[A] {
    def writes(e: A): JsValue = JsObject(Seq.empty)
    def reads(obj: JsValue): JsResult[A] = JsSuccess(instance)
  }
  implicit val receivedResponseFormat = emptyObjectFormat(ReceivedResponse())

  implicit val needRebootFormat = emptyObjectFormat(NeedRebootEvent)
  implicit val nowListeningFormat = emptyObjectFormat(NowListeningEvent)
  implicit val stoppedFormat = emptyObjectFormat(Stopped)
  implicit val requestCompletedFormat = emptyObjectFormat(RequestCompleted())
  implicit val requestFailedFormat = emptyObjectFormat(RequestFailed())
  
  implicit object outcomeFormat extends Format[TestOutcome] {
    def writes(outcome: TestOutcome): JsValue =
      JsString(outcome.toString)
    // TODO - Errors
    def reads(value: JsValue): JsResult[TestOutcome] =
      JsSuccess(TestOutcome(value.as[String]))
  }
  implicit val registerClientRequestFormat = Json.format[RegisterClientRequest]
  implicit val testEventFormat = Json.format[TestEvent]     
  implicit val executionRequestFormat = Json.format[ExecutionRequest]
  implicit val keyExecutionRequestFormat = Json.format[KeyExecutionRequest]
  implicit val executionReceivedFormat = Json.format[ExecutionRequestReceived]
  implicit val executionWaitingFormat = Json.format[ExecutionWaiting]
  implicit val executionStartingFormat = Json.format[ExecutionStarting]
  implicit val executionSuccessFormat = Json.format[ExecutionSuccess]
  implicit val executionFailureFormat = Json.format[ExecutionFailure]
  implicit val listenToEventsFormat = emptyObjectFormat(ListenToEvents())
  implicit val listenToBuildChangeFormat = emptyObjectFormat(ListenToBuildChange())
  implicit val buildStructureChangedFormat = Json.format[BuildStructureChanged]
  implicit val listenToValueFormat = Json.format[ListenToValue]
  implicit val keyNotFoundFormat = Json.format[KeyNotFound]
  implicit val compilationFailureFormat = Json.format[CompilationFailure]
  implicit val taskStartedFormat = Json.format[TaskStarted]
  implicit val taskFinishedFormat = Json.format[TaskFinished]
  implicit val readLineRequestFormat = Json.format[ReadLineRequest]
  implicit val readLineResponseFormat = Json.format[ReadLineResponse]
  implicit val confirmRequestFormat = Json.format[ConfirmRequest]
  implicit val confirmResponseFormat = Json.format[ConfirmResponse]
  implicit val keyLookupRequestFormat = Json.format[KeyLookupRequest]
  implicit val keyLookupResponseFormat = Json.format[KeyLookupResponse]
  

  // TODO - This needs an explicit format... yay.
  implicit def valueChangeHackery[A](implicit result: Format[TaskResult[A]]): Format[ValueChanged[A]] = 
    new Format[ValueChanged[A]] {
      def writes(v: ValueChanged[A]): JsValue =
        JsObject(Seq(
          "key" -> Json.toJson(v.key),
          "value" -> result.writes(v.value)
        ))
      def reads(v: JsValue): JsResult[ValueChanged[A]] = {
        for {
          key <- Json.fromJson[ScopedKey](v \ "key")
          result <- result.reads(v \ "value")
        } yield ValueChanged(key,result)
      }
    }
  
  implicit val completionFormat = Json.format[Completion]
  implicit val commandCompletionsRequestFormat = Json.format[CommandCompletionsRequest]
  implicit val commandCompletionsResponseFormat = Json.format[CommandCompletionsResponse]
}
