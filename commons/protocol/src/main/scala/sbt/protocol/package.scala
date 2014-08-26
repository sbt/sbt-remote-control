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
          case JsNull => JsSuccess(None)
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
      override def toString = "Format[Attributed[" + format + "]"
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
        if (value.isDefined) Seq(name -> format.writes(value.get)) else Nil
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
      sf: Option[java.io.File]) extends xsbti.Position {
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

  implicit val executionAnalysisCommandFormat = Json.format[ExecutionAnalysisCommand]
  implicit val executionAnalysisKeyFormat = Json.format[ExecutionAnalysisKey]
  implicit val executionAnalysisErrorFormat = Json.format[ExecutionAnalysisError]

  implicit val executionAnalysisFormat = new Format[ExecutionAnalysis] {
    override def writes(analysis: ExecutionAnalysis): JsValue = {
      val (discriminator, rest) =
        analysis match {
          case c: ExecutionAnalysisCommand => "command" -> Json.toJson(c)
          case k: ExecutionAnalysisKey => "key" -> Json.toJson(k)
          case e: ExecutionAnalysisError => "error" -> Json.toJson(e)
        }
      val baseObj = rest match {
        case o: JsObject => o
        case other => throw new RuntimeException(s"Serialized $analysis as a non-object $other")
      }
      baseObj ++ Json.obj("executionType" -> discriminator)
    }
    override def reads(v: JsValue): JsResult[ExecutionAnalysis] = {
      (v \ "executionType").validate[String] flatMap {
        case "command" => Json.fromJson[ExecutionAnalysisCommand](v)
        case "key" => Json.fromJson[ExecutionAnalysisKey](v)
        case "error" => Json.fromJson[ExecutionAnalysisError](v)
        case other => JsError(s"Invalid executionType '$other' in $v")
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

  implicit val requestCompletedFormat = emptyObjectFormat(RequestCompleted())
  implicit val requestFailedFormat = emptyObjectFormat(RequestFailed())
  implicit val killRequestFormat = emptyObjectFormat(KillServerRequest())

  implicit object outcomeFormat extends Format[TestOutcome] {
    def writes(outcome: TestOutcome): JsValue =
      JsString(outcome.toString)
    // TODO - Errors
    def reads(value: JsValue): JsResult[TestOutcome] =
      JsSuccess(TestOutcome(value.as[String]))
  }
  implicit val cancelExecutionRequestFormat = Json.format[CancelExecutionRequest]
  implicit val cancelExecutionResponseFormat = Json.format[CancelExecutionResponse]
  implicit val clientInfoFormat = Json.format[ClientInfo]
  implicit val registerClientRequestFormat = Json.format[RegisterClientRequest]
  implicit val executionRequestFormat = Json.format[ExecutionRequest]
  implicit val keyExecutionRequestFormat = Json.format[KeyExecutionRequest]
  implicit val executionReceivedFormat = Json.format[ExecutionRequestReceived]
  implicit val executionWaitingFormat = Json.format[ExecutionWaiting]
  implicit val executionStartingFormat = Json.format[ExecutionStarting]
  implicit val executionSuccessFormat = Json.format[ExecutionSuccess]
  implicit val executionFailureFormat = Json.format[ExecutionFailure]
  implicit val listenToEventsFormat = emptyObjectFormat(ListenToEvents())
  implicit val unlistenToEventsFormat = emptyObjectFormat(UnlistenToEvents())
  implicit val listenToBuildChangeFormat = emptyObjectFormat(ListenToBuildChange())
  implicit val unlistenToBuildChangeFormat = emptyObjectFormat(UnlistenToBuildChange())
  implicit val sendSyntheticBuildChangedFormat = emptyObjectFormat(SendSyntheticBuildChanged())
  implicit val buildStructureChangedFormat = Json.format[BuildStructureChanged]
  implicit val listenToValueFormat = Json.format[ListenToValue]
  implicit val unlistenToValueFormat = Json.format[UnlistenToValue]
  implicit val sendSyntheticValueChangedFormat = Json.format[SendSyntheticValueChanged]
  implicit val keyNotFoundFormat = Json.format[KeyNotFound]
  implicit val taskStartedFormat = Json.format[TaskStarted]
  implicit val taskFinishedFormat = Json.format[TaskFinished]
  implicit val readLineRequestFormat = Json.format[ReadLineRequest]
  implicit val readLineResponseFormat = Json.format[ReadLineResponse]
  implicit val confirmRequestFormat = Json.format[ConfirmRequest]
  implicit val confirmResponseFormat = Json.format[ConfirmResponse]
  implicit val keyLookupRequestFormat = Json.format[KeyLookupRequest]
  implicit val keyLookupResponseFormat = Json.format[KeyLookupResponse]
  implicit val analyzeExecutionRequestFormat = Json.format[AnalyzeExecutionRequest]
  implicit val analyzeExecutionResponseFormat = Json.format[AnalyzeExecutionResponse]
  implicit val buildLoadedFormat = emptyObjectFormat(BuildLoaded())
  implicit val buildFailedToLoadFormat = emptyObjectFormat(BuildFailedToLoad())

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  implicit val taskEventFormat: Format[TaskEvent] = new Format[TaskEvent] {
    override def writes(event: TaskEvent): JsValue = {
      Json.obj("taskId" -> event.taskId, "name" -> event.name, "serialized" -> event.serialized)
    }

    override def reads(v: JsValue): JsResult[TaskEvent] = {
      for {
        taskId <- (v \ "taskId").validate[Long]
        name <- (v \ "name").validate[String]
        serialized = (v \ "serialized")
      } yield TaskEvent(taskId = taskId, name = name, serialized = serialized)
    }
  }

  implicit def valueChangedReads[A](implicit result: Reads[TaskResult[A]]): Reads[ValueChanged[A]] = new Reads[ValueChanged[A]] {
    override def reads(v: JsValue): JsResult[ValueChanged[A]] = {
      for {
        key <- Json.fromJson[ScopedKey](v \ "key")
        result <- result.reads(v \ "value")
      } yield ValueChanged(key, result)
    }
  }

  implicit def valueChangedWrites[A](implicit result: Writes[TaskResult[A]]): Writes[ValueChanged[A]] = new Writes[ValueChanged[A]] {
    override def writes(v: ValueChanged[A]): JsValue =
      JsObject(Seq(
        "key" -> Json.toJson(v.key),
        "value" -> result.writes(v.value)))
  }

  // TODO - This needs an explicit format... yay.
  implicit def valueChangedFormat[A](implicit result: Format[TaskResult[A]]): Format[ValueChanged[A]] =
    new Format[ValueChanged[A]] {
      val reader = valueChangedReads
      val writer = valueChangedWrites
      override def writes(v: ValueChanged[A]): JsValue =
        writer.writes(v)
      override def reads(v: JsValue): JsResult[ValueChanged[A]] =
        reader.reads(v)
    }

  implicit val completionFormat = Json.format[Completion]
  implicit val commandCompletionsRequestFormat = Json.format[CommandCompletionsRequest]
  implicit val commandCompletionsResponseFormat = Json.format[CommandCompletionsResponse]

  ///// task events (do not extend protocol.Message)
  // these formatters are hand-coded because they have an unapply()
  // that confuses play-json

  implicit val testEventFormat: Format[TestEvent] = new Format[TestEvent] {
    override def writes(event: TestEvent): JsValue = {
      Json.obj("name" -> event.name, "description" -> event.description,
        "outcome" -> event.outcome, "error" -> event.error)
    }

    override def reads(v: JsValue): JsResult[TestEvent] = {
      for {
        name <- (v \ "name").validate[String]
        description <- (v \ "description").validate[Option[String]]
        outcome <- (v \ "outcome").validate[TestOutcome]
        error <- (v \ "error").validate[Option[String]]
      } yield TestEvent(name = name, description = description, outcome = outcome, error = error)
    }
  }

  implicit val compilationFailureFormat = new Format[CompilationFailure] {
    override def writes(event: CompilationFailure): JsValue = {
      Json.obj("project" -> event.project, "position" -> event.position,
        "severity" -> event.severity, "message" -> event.message)
    }

    override def reads(v: JsValue): JsResult[CompilationFailure] = {
      for {
        project <- (v \ "project").validate[ProjectReference]
        position <- (v \ "position").validate[xsbti.Position]
        severity <- (v \ "severity").validate[xsbti.Severity]
        message <- (v \ "message").validate[String]
      } yield CompilationFailure(project = project, position = position, severity = severity, message = message)
    }
  }
}
