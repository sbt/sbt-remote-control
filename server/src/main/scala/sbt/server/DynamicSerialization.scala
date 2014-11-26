package sbt
package server

import sbt.protocol._
import sbt.serialization._

/** Helper class lookups for serialization/deserialization. */
private object Classes {
  val StringClass = classOf[String]
  val FileClass = classOf[java.io.File]
  val BooleanClass = classOf[Boolean]
  val ShortClass = classOf[Short]
  val IntClass = classOf[Int]
  val LongClass = classOf[Long]
  val FloatClass = classOf[Float]
  val DoubleClass = classOf[Double]
  val OptionClass = classOf[Option[_]]
  val SeqClass = classOf[Seq[_]]
  val AttributedClass = classOf[sbt.Attributed[_]]
  val URIClass = classOf[java.net.URI]
  val ThrowableClass = classOf[Throwable]

  // TODO - Figure out how to handle attributed, and
  // other sbt special classes....

  abstract class SubClass(cls: Class[_]) {
    def unapply(ocls: Class[_]): Boolean =
      cls.isAssignableFrom(ocls)
  }

  object OptionSubClass extends SubClass(OptionClass)
  object SeqSubClass extends SubClass(SeqClass)
  object AttributedSubClass extends SubClass(AttributedClass)
  object ThrowableSubClass extends SubClass(ThrowableClass)
}

/**
 * An interface representing a mechanism to register and
 *  retrieve serialization for specific types at runtime.
 *
 *  This suffers from all the same limitations as scala.Manifest for
 *  handling types.
 *
 *  A DynamicSerialization is immutable.
 */
sealed trait DynamicSerialization {
  /** Look up a serialization using its type manifest */
  def lookup[T](implicit mf: Manifest[T]): Option[SbtSerializer[T]]
  /** Look up by runtime class (potentially broken if the class has type parameters) */
  def lookup[T](klass: Class[T]): Option[SbtSerializer[T]]
  /** Add a serializer, returning the new modified DynamicSerialization. */
  def register[T](serializer: SbtSerializer[T])(implicit mf: Manifest[T]): DynamicSerialization

  // Here we need to reflectively look up the serialization of things...
  final def buildValue[T](o: T)(implicit mf: Manifest[T]): BuildValue =
    lookup(mf) map { serializer =>
      BuildValue(JsonValue(o)(serializer.pickler), o.toString)
    } getOrElse BuildValue(JsonValue.emptyObject, o.toString)

  final def buildValueUsingRuntimeClass[T](o: T): BuildValue = {
    lookup[T](o.getClass.asInstanceOf[Class[T]]) map { serializer =>
      BuildValue(JsonValue(o)(serializer.pickler), o.toString)
    } getOrElse (BuildValue(JsonValue.emptyObject, o.toString))
  }
}

object DynamicSerialization {
  val defaultSerializations: DynamicSerialization =
    NonTrivialSerializers.registerSerializers(ConcreteDynamicSerialization(Map.empty, Map.empty))
}

private final case class ConcreteDynamicSerialization(registered: Map[Manifest[_], SbtSerializer[_]], byClass: Map[Class[_], SbtSerializer[_]]) extends DynamicSerialization {
  override def register[T](serializer: SbtSerializer[T])(implicit mf: Manifest[T]): DynamicSerialization =
    // Here we erase the original type when storing
    ConcreteDynamicSerialization(registered + (mf -> serializer), byClass + (mf.erasure -> serializer))

  override def lookup[T](implicit mf: Manifest[T]): Option[SbtSerializer[T]] =
    // When looking up, given the interface, it's safe to return to
    // the original type.
    (registered get mf).asInstanceOf[Option[SbtSerializer[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(mf)

  override def lookup[T](klass: Class[T]): Option[SbtSerializer[T]] =
    (byClass get klass).asInstanceOf[Option[SbtSerializer[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(klass)
}

private object ConcreteDynamicSerialization {
  private val defaultSerializationMemosByManifest =
    scala.collection.concurrent.TrieMap[Manifest[_], SbtSerializer[_]]()
  private val defaultSerializationMemosByClass =
    scala.collection.concurrent.TrieMap[Class[_], SbtSerializer[_]]()

  def memoizedDefaultSerializer[T](mf: Manifest[T]): Option[SbtSerializer[T]] =
    defaultSerializer(mf) match {
      case Some(s) =>
        defaultSerializationMemosByManifest.put(mf, s)
        Some(s)
      case None => None
    }

  def memoizedDefaultSerializer[T](klass: Class[T]): Option[SbtSerializer[T]] =
    defaultSerializer[T](klass) match {
      case Some(s) =>
        defaultSerializationMemosByClass.put(klass, s)
        Some(s)
      case None => None
    }

  private def defaultSerializer[T](klass: Class[T]): Option[SbtSerializer[T]] = defaultSerializationMemosByClass.get(klass).map(_.asInstanceOf[SbtSerializer[T]]) orElse {
    (klass match {
      case Classes.StringClass => Some(implicitly[SbtSerializer[String]])
      case Classes.FileClass => Some(implicitly[SbtSerializer[java.io.File]])
      case Classes.BooleanClass => Some(implicitly[SbtSerializer[Boolean]])
      case Classes.ShortClass => Some(implicitly[SbtSerializer[Short]])
      case Classes.IntClass => Some(implicitly[SbtSerializer[Int]])
      case Classes.LongClass => Some(implicitly[SbtSerializer[Long]])
      case Classes.FloatClass => Some(implicitly[SbtSerializer[Float]])
      case Classes.DoubleClass => Some(implicitly[SbtSerializer[Double]])
      case Classes.URIClass => Some(implicitly[SbtSerializer[java.net.URI]])
      case Classes.ThrowableSubClass() => Some(implicitly[SbtSerializer[java.lang.Throwable]])
      case _ =>
        None
    }).asInstanceOf[Option[SbtSerializer[T]]]
  }

  private def defaultSerializer[T](mf: Manifest[T]): Option[SbtSerializer[T]] = defaultSerializationMemosByManifest.get(mf).map(_.asInstanceOf[SbtSerializer[T]]) orElse
    defaultSerializer[T](mf.erasure.asInstanceOf[Class[T]]) orElse {
      (mf.erasure match {
        case Classes.OptionSubClass() =>
          for {
            child <- memoizedDefaultSerializer(mf.typeArguments(0))
          } yield {
            //SbtSerializer.optionWithNull(child)
            ??? /* FIXME */
          }
        // TODO - polymorphism?
        case Classes.SeqSubClass() =>
          // Now we need to find the first type arguments structure:
          import collection.generic.CanBuildFrom
          for {
            child <- memoizedDefaultSerializer(mf.typeArguments(0))
          } yield {
            //val reads = Reads.traversableReads[Seq, Any](collection.breakOut, child.asInstanceOf[Reads[Any]])
            //val writes = Writes.traversableWrites(child.asInstanceOf[Writes[Any]])
            //SbtSerializer(reads, writes)
            ??? /* FIXME */
          }
        case Classes.AttributedSubClass() =>
          for {
            child <- memoizedDefaultSerializer(mf.typeArguments(0))
          } yield ??? /* FIXME */
        case _ =>
          None
      }).asInstanceOf[Option[SbtSerializer[T]]]
    }
}

private object NonTrivialSerializers {

  private sealed trait RegisteredSbtSerializer {
    type T
    def manifest: Manifest[T]
    def serializer: SbtSerializer[T]
  }
  private def toRegisteredSbtSerializer[U](implicit s: SbtSerializer[U], mf: Manifest[U]): RegisteredSbtSerializer = new RegisteredSbtSerializer {
    type T = U
    override val serializer = s
    override val manifest = mf
  }

  /* FIXME This appears to lock up compilation forever, so somehow triggers infinite
   * macro loop or something?
   */
  def registerSerializers(base: DynamicSerialization): DynamicSerialization = {
    // TODO I think we only lookup by exact type, so registering an abstract type
    // here such as Type or xsbti.Problem is not useful. I think.
    // TODO we are registering some types here that aren't likely or conceivable
    // task results; we only need to register types T that appear in taskKey[T].
    // We don't have to register all the types of the fields in result types.
    val serializers = Seq[RegisteredSbtSerializer](
      toRegisteredSbtSerializer[ByteArray],
      toRegisteredSbtSerializer[ProjectReference],
      toRegisteredSbtSerializer[AttributeKey],
      toRegisteredSbtSerializer[SbtScope],
      toRegisteredSbtSerializer[ScopedKey],
      toRegisteredSbtSerializer[MinimalBuildStructure],
      // TODO
      // toRegisteredSbtSerializer[Analysis],
      // toRegisteredSbtSerializer[Stamps],
      // toRegisteredSbtSerializer[SourceInfo],
      // toRegisteredSbtSerializer[SourceInfos],
      // toRegisteredSbtSerializer[xsbti.Problem], */
      // toRegisteredSbtSerializer[Problem, xsbti.Problem],
      // toRegisteredSbtSerializer[APIs],
      // toRegisteredSbtSerializer[ThePackage],
      // toRegisteredSbtSerializer[TypeParameter],
      // toRegisteredSbtSerializer[Path],
      // toRegisteredSbtSerializer[Modifiers],
      // toRegisteredSbtSerializer[AnnotationArgument],
      // toRegisteredSbtSerializer[Annotation],
      // toRegisteredSbtSerializer[Definition],
      // toRegisteredSbtSerializer[SourceAPI],
      // toRegisteredSbtSerializer[Source],
      // toRegisteredSbtSerializer[RelationsSource],
      // toRegisteredSbtSerializer[Relations],
      // toRegisteredSbtSerializer[OutputSetting],
      // toRegisteredSbtSerializer[Compilation],
      // toRegisteredSbtSerializer[Compilations],
      // toRegisteredSbtSerializer[Stamp],
      // toRegisteredSbtSerializer[Qualifier],
      // toRegisteredSbtSerializer[Access],
      // toRegisteredSbtSerializer[PathComponent],
      // toRegisteredSbtSerializer[Type],
      // toRegisteredSbtSerializer[SimpleType, Type],
      toRegisteredSbtSerializer[CompileFailedException],
      toRegisteredSbtSerializer[ModuleId])
    serializers.foldLeft(base) { (sofar, next) =>
      sofar.register(next.serializer)(next.manifest)
    }
  }
}
