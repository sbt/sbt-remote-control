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
  implicit object ProjectInfoStructure extends RawStructure[ProjectInfo] {
    override def apply(t: ProjectInfo): Map[String, Any] =
        Map("name" -> t.name,
            "ref" -> ProjectReference.MyStructure(t.ref),
            "default" -> t.default,
            "attributes" -> t.attributes)
    override def unapply(map: Map[String, Any]): Option[ProjectInfo] =
      // TODO - All of this optional?
     ProjectReference.MyStructure.unapply(map("ref").asInstanceOf[Map[String,Any]]).map { ref => 
       ProjectInfo(
    	 ref,
         map("name").asInstanceOf[String],
         map("default").asInstanceOf[Boolean],
         map("attributes").asInstanceOf[Map[String,Any]]
       )
     }
  }
  implicit object NameResponseStructure extends RawStructure[NameResponse] {
    private val s = RawStructure.get[Seq[ProjectInfo]]
    override def apply(t: NameResponse): Map[String, Any] =
        Map("response" -> "NameResponse",
            "projects" -> s(t.projects))
    override def unapply(map: Map[String, Any]): Option[NameResponse] =
      // TODO - All of this optional?
     map.get("response").filter(_ == "NameResponse").map { _ => 
       NameResponse(
         s.unapply(map("projects").asInstanceOf[Map[String, Any]]).getOrElse {
           sys.error("Unable to parse Project Infos: " + map)
         }    
       )
     }
  }
  
  abstract class RequestWithSendEventsAndRefStructure[T <: RequestOnProject](name: String, creator: (Boolean, Option[ProjectReference]) => T) extends RawStructure[T] {
    override def apply(t: T): Map[String, Any] = {
        Map("request" -> name,
            "sendEvents" -> t.sendEvents) ++
            t.ref.map(x => "ref" -> ProjectReference.MyStructure(x))
     }
            
    override def unapply(map: Map[String, Any]): Option[T] =
      map.get("request").filter(_ == name).map { _ => 
        creator(
            map("sendEvents").asInstanceOf[Boolean],
            map.get("ref").map { x =>
              ProjectReference.MyStructure.unapply(x.asInstanceOf[Map[String,Any]]).getOrElse {
                sys.error("Could not deserialize project reference.")
              }
            }    
        )
      }
  }
  
  implicit object mainClassRequestStructure extends 
    RequestWithSendEventsAndRefStructure[MainClassRequest]("MainClassRequest", MainClassRequest.apply _)
    
  implicit object DiscoveredMainClassesStructure extends RawStructure[DiscoveredMainClasses] {
    
    def apply(msg: DiscoveredMainClasses): Map[String, Any] = {
      Map("project" -> ProjectReference.MyStructure(msg.project),
          "mainClasses" -> msg.mainClasses) ++
          msg.defaultMainClass.map("default" -> _)
    }
    def unapply(obj: Map[String,Any]): Option[DiscoveredMainClasses] = {
      for {
        pObj <- obj get "project"
        p <- ProjectReference.MyStructure.unapply(pObj.asInstanceOf[Map[String, Any]])
        mcObj <- obj get "mainClasses"
        mc = mcObj.asInstanceOf[Seq[String]]
        d = (obj get "default").asInstanceOf[Option[String]]
      } yield DiscoveredMainClasses(p, mc, d)
    }
  }
  implicit object mainClassResponseStructure extends RawStructure[MainClassResponse] {
    def apply(msg: MainClassResponse): Map[String, Any] = {
      Map("response" -> "MainClassResponse",
          "projects" -> msg.projects.map(DiscoveredMainClassesStructure.apply))
    }
    def unapply(obj: Map[String,Any]): Option[MainClassResponse] = {
      obj.get("response").filter(_ == "MainClassResponse").map { _ =>
        val projects = 
          obj("projects").asInstanceOf[Seq[Map[String, Any]]].map { obj =>
            DiscoveredMainClassesStructure.unapply(obj).getOrElse {
              sys.error("Could not deserialize main class information.")
            }
          }
        MainClassResponse(projects)
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
    RequestWithSendEventsAndRefStructure[CompileRequest]("CompileRequest", CompileRequest.apply _)
  implicit object CompileResultStructure extends RawStructure[CompileResult] {
    def apply(msg: CompileResult): Map[String, Any] = {
      Map("ref" -> ProjectReference.MyStructure(msg.ref), 
          "success" -> msg.success)
    }
    def unapply(obj: Map[String,Any]): Option[CompileResult] = {
      for {
        xObj <- obj get "ref"
        ref <- ProjectReference.MyStructure.unapply(xObj.asInstanceOf[Map[String,Any]])
        success <- obj get "success"
      } yield CompileResult(ref, success.asInstanceOf[Boolean])
    }
  } 
  implicit object CompileResponseStructure extends RawStructure[CompileResponse] {
    def apply(msg: CompileResponse): Map[String, Any] = {
      Map("response" -> "CompileResponse",
          "results" -> msg.results.map(CompileResultStructure.apply))
    }
    def unapply(obj: Map[String,Any]): Option[CompileResponse] = {
      obj.get("response").filter(_ == "CompileResponse").map { _ =>
        val results = 
          obj("results").asInstanceOf[Seq[Map[String, Any]]].map(x => CompileResultStructure.unapply(x).getOrElse {
            sys.error("Could not deserialize compile result: " + x)
          })
        CompileResponse(results)
      }
    }
  }
  
  implicit object RunRequestStructure extends RawStructure[RunRequest] {
    def apply(msg: RunRequest): Map[String, Any] = {
      Map("request" -> "RunRequest",
          "useAtmos" -> msg.useAtmos,
          "sendEvents" -> msg.sendEvents) ++
          msg.mainClass.map("mainClass" -> _) ++
          msg.ref.map { p =>
            "ref" -> ProjectReference.MyStructure(p)
          }
    }
    def unapply(obj: Map[String,Any]): Option[RunRequest] = {
      obj.get("request").filter(_ == "RunRequest").map { _ =>
        
        val ref =
          for {
            rObj <- obj get "ref"
            r <- ProjectReference.MyStructure.unapply(rObj.asInstanceOf[Map[String,Any]])
          } yield r
        
        RunRequest(obj("sendEvents").asInstanceOf[Boolean],
            ref,
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
    RequestWithSendEventsAndRefStructure[TestRequest]("TestRequest", TestRequest.apply _)
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
  implicit object StartedEventStructure extends RawStructure[Started.type] {
    def apply(msg: Started.type) =
      Map("event" -> "Started")
    def unapply(obj: Map[String,Any]): Option[Started.type] = 
      obj.get("event").filter(_ == "Started").map(_ => Started)
  }
  implicit object StoppedEventStructure extends RawStructure[Stopped.type] {
    def apply(msg: Stopped.type) =
      Map("event" -> "Stopped")
    def unapply(obj: Map[String,Any]): Option[Stopped.type] = 
      obj.get("event").filter(_ == "Stopped").map(_ => Stopped)
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
  implicit object TestEventStructure extends RawStructure[TestEvent] {
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
   
   
   
   // Generic API
   implicit object ExecutionRequestStructure extends RawStructure[ExecutionRequest] {
    def apply(msg: ExecutionRequest) = (
      Map("request" -> "ExecutionRequest",
          "command" -> msg.command)
    )
    def unapply(obj: Map[String,Any]): Option[ExecutionRequest] = {
      obj.get("request").filter(_ == "ExecutionRequest").map { _ =>
        ExecutionRequest(obj("command").asInstanceOf[String])
      }
    }
  }
  implicit object ExecutionDoneStructure extends RawStructure[ExecutionDone] {
    def apply(msg: ExecutionDone) =
      Map("event" -> "ExecutionDone", "command" -> msg.command)
    def unapply(obj: Map[String,Any]): Option[ExecutionDone] = {
      obj.get("event").filter(_ == "ExecutionDone").map { _ =>
        ExecutionDone(obj("command").toString)
      }
    }
  }
  implicit object ListenToEventsStructure extends RawStructure[ListenToEvents] {
    def apply(msg: ListenToEvents) = 
      Map("request" -> "ListenToEvents")
    def unapply(obj: Map[String,Any]): Option[ListenToEvents] = {
      obj.get("request").filter(_ == "ListenToEvents").map { _ =>
        ListenToEvents()
      }
    }
  }
  implicit object ListenToBuildChangeStructure extends RawStructure[ListenToBuildChange] {
    def apply(msg: ListenToBuildChange) = 
      Map("request" -> "ListenToBuildChange")
    def unapply(obj: Map[String,Any]): Option[ListenToBuildChange] = {
      obj.get("request").filter(_ == "ListenToBuildChange").map { _ =>
        ListenToBuildChange()
      }
    }
  }
  
  implicit object BuildStructureChangedStructure extends RawStructure[BuildStructureChanged] {
    val InternalStruct = MinimalBuildStructure.MyStructure
    def apply(msg: BuildStructureChanged) = 
      Map("event" -> "BuildStructureChanged", "build" -> InternalStruct(msg.structure))
    def unapply(obj: Map[String,Any]): Option[BuildStructureChanged] = {
      obj.get("event").filter(_ == "BuildStructureChanged").flatMap { _ =>
        InternalStruct.unapply(obj("build").asInstanceOf[Map[String,Any]]).map(BuildStructureChanged.apply)
      }
    }
  }
   
  implicit object ListenToValueStructure extends RawStructure[ListenToValue] {
    val InternalStruct = ScopedKey.MyStructure
    def apply(msg: ListenToValue) = 
      Map("request" -> "ListenToValue", "key" -> InternalStruct(msg.key))
    def unapply(obj: Map[String,Any]): Option[ListenToValue] = {
      obj.get("request").filter(_ == "ListenToValue").flatMap { _ =>
        InternalStruct.unapply(obj("key").asInstanceOf[Map[String,Any]]).map(ListenToValue.apply)
      }
    }
  }
  
  implicit object CompilationFailureStructure extends RawStructure[CompilationFailure] {
    val PositionStruct = RawStructure.get[xsbti.Position]
    def apply(cf: CompilationFailure): Map[String,Any] = {
      Map(
        "event" -> "CompilationFailure",
        "project" -> ProjectReference.MyStructure(cf.project),
        "position" -> PositionStruct(cf.position),
        "severity" -> cf.severity.name,
        "message" -> cf.msg
      )
    }
    override def unapply(m: Map[String,Any]): Option[CompilationFailure] = 
      m.get("event").filter(_ == "CompilationFailure").map { _ =>
        // TODO - good error messages?
        val project = ProjectReference.MyStructure.unapply(m("project").asInstanceOf[Map[String,Any]]).get
        val position = PositionStruct.unapply(m("position").asInstanceOf[Map[String,Any]]).get
        val severity = xsbti.Severity.valueOf(m("severity").toString)
        val message = m("message").toString
        CompilationFailure(project, position, severity, message)
      }
     // xsbti.Severity.valueOf(msg)
  }
  
  implicit object TaskStartedStructure extends RawStructure[TaskStarted] {
    def apply(msg: TaskStarted): Map[String,Any] =
      Map(
        "event" -> "TaskStarted",
        "key" -> ScopedKey.MyStructure(msg.key)
      )
    def unapply(in: Map[String, Any]): Option[TaskStarted] =
      for {
        name <- in.get("event")
        if name == "TaskStarted"
        key <- ScopedKey.MyStructure.unapply(in("key").asInstanceOf[Map[String,Any]])
      } yield TaskStarted(key)
    
  }
  implicit object TaskStoppedStructure extends RawStructure[TaskFinished] {
    def apply(msg: TaskFinished): Map[String,Any] =
      Map(
        "event" -> "TaskFinished",
        "key" -> ScopedKey.MyStructure(msg.key),
        "success" -> msg.success
      )
    def unapply(in: Map[String, Any]): Option[TaskFinished] =
      for {
        name <- in.get("event")
        if name == "TaskFinished"
        key <- ScopedKey.MyStructure.unapply(in("key").asInstanceOf[Map[String,Any]])
        success = in("success").asInstanceOf[Boolean]
      } yield TaskFinished(key, success)
    
  }
}