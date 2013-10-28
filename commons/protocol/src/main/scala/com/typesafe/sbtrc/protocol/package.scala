package com.typesafe.sbtrc

package object protocol {
  // Protocol serializers...
  
  // Low-level API
  implicit object cancelRequestStructure extends RawStructure[CancelRequest.type] {
    override def apply(t: CancelRequest.type): Map[String, Any] =
        Map("request" -> "CancelRequest")
    override def unapply(map: Map[String, Any]): Option[CancelRequest.type] =
        map.get("request").filter(_ == "CancelRequest").map(_ => CancelRequest) 
  }
  implicit object cancelResponseStructure extends RawStructure[CancelResponse.type] {
    override def apply(t: CancelResponse.type): Map[String, Any] =
        Map("response" -> "CancelResponse")
    override def unapply(map: Map[String, Any]): Option[CancelResponse.type] =
        map.get("response").filter(_ == "CancelResponse").map(_ => CancelResponse) 
  }
  implicit object errorResponseStructure extends RawStructure[ErrorResponse] {
    override def apply(t: ErrorResponse): Map[String, Any] =
        Map("response" -> "ErrorResponse", 
            "error" -> t.error)
    override def unapply(map: Map[String, Any]): Option[ErrorResponse] =
        map.get("response").filter(_ == "ErrorResponse").map(_ => ErrorResponse(map("error").asInstanceOf[String]))
  }
  
  abstract class AbstractKeyRequestStructure[T <: KeyRequest](name: String, Make: KeyFilter => T) extends RawStructure[T] {
    override def apply(t: T): Map[String, Any] = {
      Map("request" -> name,
          "filter" -> KeyFilter.MyStructure(t.filter))
    }
    override def unapply(obj: Map[String,Any]): Option[T] = {
      obj.get("request").filter(_ == name).map { _ =>
        val rawFilter = obj("filter").asInstanceOf[Map[String,Any]]
        Make(
           KeyFilter.MyStructure.unapply(rawFilter).getOrElse {
             sys.error("Unable to extract key filter from KeyRequest")
           }
        )  
      }
    }
  }
  
  implicit object SettingKeyRequestStructure extends AbstractKeyRequestStructure[SettingKeyRequest](
         "SettingKeyRequest", 
         SettingKeyRequest.apply _)  
  implicit object TaskKeyRequestStructure extends AbstractKeyRequestStructure[TaskKeyRequest](
         "TaskKeyRequest", 
         TaskKeyRequest.apply _)
  implicit object InputTaskKeyRequestStructure extends AbstractKeyRequestStructure[InputTaskKeyRequest](
         "InputTaskKeyRequest", 
         InputTaskKeyRequest.apply _)
         
  implicit object SettingValueRequestStructure extends RawStructure[SettingValueRequest] {
    override def apply(t: SettingValueRequest): Map[String, Any] = {
      Map("request" -> "SettingValueRequest",
          "key" -> ScopedKey.MyStructure(t.key))
    }
    override def unapply(obj: Map[String,Any]): Option[SettingValueRequest] = {
      obj.get("request").filter(_ == "SettingValueRequest").map { _ =>
        val rawKey = obj("key").asInstanceOf[Map[String,Any]]
        SettingValueRequest(
           ScopedKey.MyStructure.unapply(rawKey).getOrElse {
             sys.error("Unable to extract key from SettingValueRequest = " + obj)
           }
        )  
      }
    }
  }
  implicit def SettingValueResponseStructure[T](implicit p: RawStructure[BuildValue[T]]) = new RawStructure[SettingValueResponse[T]] {
    override def apply(t: SettingValueResponse[T]): Map[String, Any] = {
      Map("response" -> "SettingValueResponse",
          "value" -> TaskResult.MyStructure[T].apply(t.value))
    }
    override def unapply(obj: Map[String,Any]): Option[SettingValueResponse[T]] = {
      obj.get("response").filter(_ == "SettingValueResponse").map { _ =>
        val rawValue = obj("value").asInstanceOf[Map[String,Any]]
        SettingValueResponse[T](
           TaskResult.MyStructure[T].unapply(rawValue).getOrElse {
             sys.error("Unable to extract key from SettingValueRequest")
           }
        )  
      }
    }
  }
  implicit object TaskValueRequestStructure extends RawStructure[TaskValueRequest] {
    override def apply(t: TaskValueRequest): Map[String, Any] = {
      Map("request" -> "TaskValueRequest",
          "key" -> ScopedKey.MyStructure(t.key),
          "sendEvents" -> t.sendEvents)
    }
    override def unapply(obj: Map[String,Any]): Option[TaskValueRequest] = {
      obj.get("request").filter(_ == "TaskValueRequest").map { _ =>
        val rawKey = obj("key").asInstanceOf[Map[String,Any]]
        TaskValueRequest(
           ScopedKey.MyStructure.unapply(rawKey).getOrElse {
             sys.error("Unable to extract key from SettingValueRequest")
           },
           obj("sendEvents").asInstanceOf[Boolean]
        )  
      }
    }
  }
  implicit def TaskValueResponseStructure[T](implicit p: RawStructure[BuildValue[T]]) = new RawStructure[TaskValueResponse[T]] {
    override def apply(t: TaskValueResponse[T]): Map[String, Any] = {
      Map("response" -> "TaskValueResponse",
          "value" -> TaskResult.MyStructure[T].apply(t.value))
    }
    override def unapply(obj: Map[String,Any]): Option[TaskValueResponse[T]] = {
      obj.get("response").filter(_ == "TaskValueResponse").map { _ =>
        val rawValue = obj("value").asInstanceOf[Map[String,Any]]
        TaskValueResponse[T](
           TaskResult.MyStructure[T].unapply(rawValue).getOrElse {
             sys.error("Unable to extract key from SettingValueRequest")
           }
        )  
      }
    }
  }
  implicit object KeyListResponseStructure extends RawStructure[KeyListResponse] {
    override def apply(t: KeyListResponse): Map[String, Any] = {
      Map("response" -> "KeyListResponse",
          "keys" -> KeyList.MyStructure(t.keys))
    }
    override def unapply(obj: Map[String,Any]): Option[KeyListResponse] = {
      obj.get("response").filter(_ == "KeyListResponse").map { _ =>
        val rawKeys = obj("keys").asInstanceOf[Map[String,Any]]
        KeyListResponse(
           KeyList.MyStructure.unapply(rawKeys).getOrElse {
             sys.error("Unable to extract key from SettingValueRequest")
           })  
      }
    }
  }
  implicit object ExecuteCommandRequestStructure extends RawStructure[ExecuteCommandRequest] {
    override def apply(t: ExecuteCommandRequest): Map[String, Any] = {
      Map("request" -> "ExecuteCommandRequest",
          "command" -> t.command,
          "sendEvents" -> t.sendEvents)
    }
    override def unapply(obj: Map[String,Any]): Option[ExecuteCommandRequest] = {
      obj.get("request").filter(_ == "ExecuteCommandRequest").map { _ =>
        ExecuteCommandRequest(
           obj("command").asInstanceOf[String],
           obj("sendEvents").asInstanceOf[Boolean]
        )  
      }
    }
  }
    implicit object ExecuteCommandResponseStructure extends RawStructure[ExecuteCommandResponse] {
    override def apply(t: ExecuteCommandResponse): Map[String, Any] = {
      Map("response" -> "ExecuteCommandResponse")
    }
    override def unapply(obj: Map[String,Any]): Option[ExecuteCommandResponse] = {
      obj.get("response").filter(_ == "ExecuteCommandResponse").map { _ =>
        ExecuteCommandResponse()  
      }
    }
  }
  
  // High Level API
  
  abstract class RequestWithSendEventsStructure[T <: Request](name: String, creator: Boolean => T) extends RawStructure[T] {
    override def apply(t: T): Map[String, Any] = {
        Map("request" -> name, 
            "sendEvents" -> t.sendEvents)
     }
            
    override def unapply(map: Map[String, Any]): Option[T] =
      map.get("request").filter(_ == name).map { _ => 
        creator(map("sendEvents").asInstanceOf[Boolean])
      }
  }
  
  implicit object nameRequestStructure extends RequestWithSendEventsStructure[NameRequest]("NameRequest", NameRequest.apply _)
  implicit object nameResponseStructure extends RawStructure[NameResponse] {
    override def apply(t: NameResponse): Map[String, Any] =
        Map("response" -> "NameResponse", 
            "name" -> t.name,
            "attributes" -> t.attributes)
    override def unapply(map: Map[String, Any]): Option[NameResponse] =
     map.get("response").filter(_ == "NameResponse").map { _ => 
       NameResponse(
         map("name").asInstanceOf[String],
         map("attributes").asInstanceOf[Map[String,Any]]
       )
     }
  }
  
  implicit object mainClassRequestStructure extends 
    RequestWithSendEventsStructure[MainClassRequest]("MainClassRequest", MainClassRequest.apply _)
  implicit object mainClassResponseStructure extends RawStructure[MainClassResponse] {
    def apply(msg: MainClassResponse): Map[String, Any] = {
      Map("response" -> "MainClassResponse") ++
      msg.name.map("name" -> _)
    }
    def unapply(obj: Map[String,Any]): Option[MainClassResponse] = {
      obj.get("response").filter(_ == "MainClassResponse").map { _ =>
        MainClassResponse(obj.get("name").asInstanceOf[Option[String]])  
      }
    }
  }
    
    
  implicit object DiscoveredMainClassesRequestStructure extends 
    RequestWithSendEventsStructure[DiscoveredMainClassesRequest]("DiscoveredMainClassesRequest", DiscoveredMainClassesRequest.apply _)
  implicit object DiscoveredMainClassesResponseStructure extends RawStructure[DiscoveredMainClassesResponse] {
    def apply(msg: DiscoveredMainClassesResponse): Map[String, Any] = {
      Map("response" -> "DiscoveredMainClassesResponse",
          "names" -> msg.names)
    }
    def unapply(obj: Map[String,Any]): Option[DiscoveredMainClassesResponse] = {
      obj.get("response").filter(_ == "DiscoveredMainClassesResponse").map { _ =>
        DiscoveredMainClassesResponse(obj("names").asInstanceOf[Seq[String]])  
      }
    }
  }
    
    
  implicit object WatchTransitiveSourcesRequestStructure extends 
    RequestWithSendEventsStructure[WatchTransitiveSourcesRequest]("WatchTransitiveSourcesRequest", WatchTransitiveSourcesRequest.apply _)    
  implicit object WatchTransitiveSourcesResponseStructure extends RawStructure[WatchTransitiveSourcesResponse] {
    private val files = RawStructure.get[Seq[java.io.File]]
    def apply(msg: WatchTransitiveSourcesResponse): Map[String, Any] = {
      Map("response" -> "WatchTransitiveSourcesResponse",
          "files" -> files(msg.files))
    }
    def unapply(obj: Map[String,Any]): Option[WatchTransitiveSourcesResponse] = {
      obj.get("response").filter(_ == "WatchTransitiveSourcesResponse").map { _ =>
        WatchTransitiveSourcesResponse(
          files.unapply(obj("files").asInstanceOf[Map[String,Any]]).getOrElse {
            sys.error("Unable to parse files of WatchTransitiveSourcesResponse")
          }    
        )  
      }
    }
  }
    
  implicit object CompileRequestStructure extends 
    RequestWithSendEventsStructure[CompileRequest]("CompileRequest", CompileRequest.apply _)    
  implicit object CompileResponseStructure extends RawStructure[CompileResponse] {
    def apply(msg: CompileResponse): Map[String, Any] = {
      Map("response" -> "CompileResponse",
          "success" -> msg.success)
    }
    def unapply(obj: Map[String,Any]): Option[CompileResponse] = {
      obj.get("response").filter(_ == "CompileResponse").map { _ =>
        CompileResponse(obj("success").asInstanceOf[Boolean])  
      }
    }
  }
  
  implicit object RunRequestStructure extends RawStructure[RunRequest] {
    def apply(msg: RunRequest): Map[String, Any] = {
      Map("request" -> "RunRequest",
          "useAtmos" -> msg.useAtmos,
          "sendEvents" -> msg.sendEvents) ++
          msg.mainClass.map("mainClass" -> _)
    }
    def unapply(obj: Map[String,Any]): Option[RunRequest] = {
      obj.get("request").filter(_ == "RunRequest").map { _ =>
        RunRequest(obj("sendEvents").asInstanceOf[Boolean],
            obj.get("mainClass").asInstanceOf[Option[String]],
            obj("useAtmos").asInstanceOf[Boolean])  
      }
    }
  }
  implicit object RunResponseStructure extends RawStructure[RunResponse] {
    def apply(msg: RunResponse): Map[String, Any] = {
      Map("response" -> "RunResponse",
          "success" -> msg.success,
          "task" -> msg.task
          )
    }
    def unapply(obj: Map[String,Any]): Option[RunResponse] = {
      obj.get("response").filter(_ == "RunResponse").map { _ =>
        RunResponse(
            obj("success").asInstanceOf[Boolean],
            obj("task").asInstanceOf[String])  
      }
    }
  }
  implicit object TestRequestStructure extends 
    RequestWithSendEventsStructure[TestRequest]("TestRequest", TestRequest.apply _)
  implicit object TestResponseStructure extends RawStructure[TestResponse] {
    def apply(msg: TestResponse): Map[String, Any] = {
      Map("response" -> "TestResponse",
          "outcome" -> msg.outcome.toString
          )
    }
    def unapply(obj: Map[String,Any]): Option[TestResponse] = {
      obj.get("response").filter(_ == "TestResponse").map { _ =>
        TestResponse(TestOutcome(obj("outcome").asInstanceOf[String]))
      }
    }
  }
    
  // EVENTS
  
  implicit object LogEntryStructure extends RawStructure[LogEntry] {
    def apply(entry: LogEntry): Map[String, Any] = 
      entry match {
        case LogSuccess(message) =>
          Map("type" -> "success", "message" -> message)
        case LogTrace(klass, message) =>
          Map("type" -> "trace", "class" -> klass, "message" -> message)
        case LogMessage(level, message) =>
          Map("type" -> "message", "level" -> level, "message" -> message)
        case LogStdOut(message) =>
          Map("type" -> "stdout", "message" -> message)
        case LogStdErr(message) =>
          Map("type" -> "stderr", "message" -> message)

      }
    def unapply(obj: Map[String, Any]): Option[LogEntry] = {
      obj.get("type").collect {
        case "success" => LogSuccess(obj("message").asInstanceOf[String])
        case "trace" => LogTrace(obj("class").asInstanceOf[String], obj("message").asInstanceOf[String])
        case "message" => LogMessage(obj("level").asInstanceOf[String], obj("message").asInstanceOf[String])
        case "stdout" => LogStdOut(obj("message").asInstanceOf[String])
        case "stderr" => LogStdErr(obj("message").asInstanceOf[String])
      }
    }
  }
  implicit object LogEventStructure extends RawStructure[LogEvent] {
    def apply(msg: LogEvent) =
      Map("event" -> "LogEvent",
          "entry" -> LogEntryStructure(msg.entry)
      )
    def unapply(obj: Map[String,Any]): Option[LogEvent] = {
      obj.get("event").filter(_ == "LogEvent").flatMap { _ =>
        LogEntryStructure.unapply(obj("entry").asInstanceOf[Map[String,Any]]).map(LogEvent(_))
      }
    }
  }
  implicit object NeedRebootEventEventStructure extends RawStructure[NeedRebootEvent.type] {
    def apply(msg: NeedRebootEvent.type) =
      Map("event" -> "NeedRebootEvent")
    def unapply(obj: Map[String,Any]): Option[NeedRebootEvent.type] = 
      obj.get("event").filter(_ == "NeedRebootEvent").map(_ => NeedRebootEvent)
  } 
  implicit object NowListeningEventStructure extends RawStructure[NowListeningEvent.type] {
    def apply(msg: NowListeningEvent.type) =
      Map("event" -> "NowListeningEvent")
    def unapply(obj: Map[String,Any]): Option[NowListeningEvent.type] = 
      obj.get("event").filter(_ == "NowListeningEvent").map(_ => NowListeningEvent)
  } 
  implicit object RequestReceivedEventStructure extends RawStructure[RequestReceivedEvent.type] {
    def apply(msg: RequestReceivedEvent.type) =
      Map("event" -> "RequestReceivedEvent")
    def unapply(obj: Map[String,Any]): Option[RequestReceivedEvent.type] = 
      obj.get("event").filter(_ == "RequestReceivedEvent").map(_ => RequestReceivedEvent)
  }
  implicit object TestEventStrcuture extends RawStructure[TestEvent] {
    def apply(msg: TestEvent) = (
      Map("event" -> "TestEvent",
          "name" -> msg.name,
          "outcome" -> msg.outcome.toString)
          ++ msg.description.map(d => "description" -> d)
          ++ msg.error.map(e => "error" -> e) 
    )
    def unapply(obj: Map[String,Any]): Option[TestEvent] = {
      obj.get("event").filter(_ == "TestEvent").map { _ =>
        TestEvent(
          name = obj("name").asInstanceOf[String],
          outcome = TestOutcome(obj("outcome").asInstanceOf[String]),
          description = obj.get("description").asInstanceOf[Option[String]],
          error = obj.get("error").asInstanceOf[Option[String]]
        )
      }
    }
  }
      
   implicit object GenericEventStructure extends RawStructure[GenericEvent] {
     def apply(msg: GenericEvent) = (
      Map("event" -> msg.id) ++ msg.params 
    )
    def unapply(obj: Map[String,Any]): Option[GenericEvent] = {
      obj.get("event").map { id =>
        GenericEvent(id.toString, obj.filterKeys(_ != "event"))
      }
   }
   }
}