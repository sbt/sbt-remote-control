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
  val VectorClass = classOf[Vector[_]]
  val ListClass = classOf[List[_]]
  val SeqClass = classOf[Seq[_]]
  val NilClass = Nil.getClass
  val URIClass = classOf[java.net.URI]
  val ThrowableClass = classOf[Throwable]

  // TODO - Figure out how to handle attributed, and
  // other sbt special classes....

  abstract class SubClass(cls: Class[_]) {
    def unapply(ocls: Class[_]): Boolean =
      cls.isAssignableFrom(ocls)
  }

  object OptionSubClass extends SubClass(OptionClass)
  object VectorSubClass extends SubClass(VectorClass)
  object ListSubClass extends SubClass(ListClass)
  object SeqSubClass extends SubClass(SeqClass)
  object ThrowableSubClass extends SubClass(ThrowableClass)

  object protocol {
    val Attributed = classOf[sbt.protocol.Attributed[_]]
  }
}

// placeholder value for not serializable task results;
// TODO we should use Unit and a unitPickler, I think.
private case class TransientValue()
private object TransientValue {
  implicit val pickler = Pickler.generate[TransientValue]
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
  def lookup[T](implicit mf: Manifest[T]): Option[Pickler[T] with Unpickler[T]]
  /** Look up by runtime class (potentially broken if the class has type parameters) */
  def lookup[T](klass: Class[T]): Option[Pickler[T] with Unpickler[T]]
  /** Add a serializer, returning the new modified DynamicSerialization. */
  def register[T](serializer: Pickler[T] with Unpickler[T])(implicit mf: Manifest[T]): DynamicSerialization

  // Here we need to reflectively look up the serialization of things...
  final def buildValue[T](o: T)(implicit mf: Manifest[T]): BuildValue =
    lookup(mf) map { serializer =>
      BuildValue(SerializedValue(o)(serializer), o.toString)
    } getOrElse {
      System.err.println(s"No dynamic serializer found (using manifest $mf) for $o")
      BuildValue(SerializedValue(TransientValue()), o.toString)
    }

  final def buildValueUsingRuntimeClass[T](o: T): BuildValue = {
    lookup[T](o.getClass.asInstanceOf[Class[T]]) map { serializer =>
      BuildValue(SerializedValue(o)(serializer), o.toString)
    } getOrElse {
      System.err.println(s"No dynamic serializer found (using runtime class ${o.getClass.getName}) for $o")
      BuildValue(SerializedValue(TransientValue()), o.toString)
    }
  }
}

object DynamicSerialization {
  val defaultSerializations: DynamicSerialization =
    NonTrivialSerializers.registerSerializers(ConcreteDynamicSerialization(Map.empty, Map.empty))
}

private final case class ConcreteDynamicSerialization(registered: Map[Manifest[_], Pickler[_] with Unpickler[_]], byClass: Map[Class[_], Pickler[_] with Unpickler[_]]) extends DynamicSerialization {
  override def register[T](serializer: Pickler[T] with Unpickler[T])(implicit mf: Manifest[T]): DynamicSerialization =
    // Here we erase the original type when storing
    ConcreteDynamicSerialization(registered + (mf -> serializer), byClass + (mf.runtimeClass -> serializer))

  override def lookup[T](implicit mf: Manifest[T]): Option[Pickler[T] with Unpickler[T]] =
    // When looking up, given the interface, it's safe to return to
    // the original type.
    (registered get mf).asInstanceOf[Option[Pickler[T] with Unpickler[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(mf)

  override def lookup[T](klass: Class[T]): Option[Pickler[T] with Unpickler[T]] =
    (byClass get klass).asInstanceOf[Option[Pickler[T] with Unpickler[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(klass)
}

private object ConcreteDynamicSerialization {
  private val defaultSerializationMemosByManifest =
    scala.collection.concurrent.TrieMap[Manifest[_], Pickler[_] with Unpickler[_]]()
  private val defaultSerializationMemosByClass =
    scala.collection.concurrent.TrieMap[Class[_], Pickler[_] with Unpickler[_]]()

  def memoizedDefaultSerializer[T](mf: Manifest[T]): Option[Pickler[T] with Unpickler[T]] =
    defaultSerializer(mf) match {
      case Some(s) =>
        defaultSerializationMemosByManifest.put(mf, s)
        Some(s)
      case None => None
    }

  def memoizedDefaultSerializer[T](klass: Class[T]): Option[Pickler[T] with Unpickler[T]] =
    defaultSerializer[T](klass) match {
      case Some(s) =>
        defaultSerializationMemosByClass.put(klass, s)
        Some(s)
      case None => None
    }

  private def makeSerializer[T](implicit pickler: Pickler[T], unpickler: Unpickler[T]): Option[Pickler[T] with Unpickler[T]] = {
    Some(PicklerUnpickler[T](pickler, unpickler))
  }

  private def defaultSerializer[T](klass: Class[T]): Option[Pickler[T] with Unpickler[T]] = defaultSerializationMemosByClass.get(klass).map(_.asInstanceOf[Pickler[T] with Unpickler[T]]) orElse {
    (klass match {
      case Classes.NilClass => makeSerializer[List[String]] // String is arbitrary
      case Classes.StringClass => makeSerializer[String]
      case Classes.FileClass => makeSerializer[java.io.File]
      case Classes.BooleanClass => makeSerializer[Boolean]
      case Classes.ShortClass => makeSerializer[Short]
      case Classes.IntClass => makeSerializer[Int]
      case Classes.LongClass => makeSerializer[Long]
      case Classes.FloatClass => makeSerializer[Float]
      case Classes.DoubleClass => makeSerializer[Double]
      case Classes.URIClass => makeSerializer[java.net.URI]
      case Classes.ThrowableSubClass() => makeSerializer[java.lang.Throwable]
      case _ =>
        None
    }).asInstanceOf[Option[Pickler[T] with Unpickler[T]]]
  }

  private def defaultSerializerForOption[T](klass: Class[T]): Option[Pickler[Option[T]] with Unpickler[Option[T]]] = {
    (klass match {
      case Classes.StringClass => makeSerializer[Option[String]]
      case Classes.FileClass => makeSerializer[Option[java.io.File]]
      case Classes.BooleanClass => makeSerializer[Option[Boolean]]
      case Classes.ShortClass => makeSerializer[Option[Short]]
      case Classes.IntClass => makeSerializer[Option[Int]]
      case Classes.LongClass => makeSerializer[Option[Long]]
      case Classes.FloatClass => makeSerializer[Option[Float]]
      case Classes.DoubleClass => makeSerializer[Option[Double]]
      case Classes.URIClass => makeSerializer[Option[java.net.URI]]
      case Classes.ThrowableSubClass() => makeSerializer[Option[java.lang.Throwable]]
      case _ =>
        None
    }).asInstanceOf[Option[Pickler[Option[T]] with Unpickler[Option[T]]]]
  }

  private def defaultSerializerForVector[T](klass: Class[T]): Option[Pickler[Vector[T]] with Unpickler[Vector[T]]] = {
    (klass match {
      case Classes.StringClass => makeSerializer[Vector[String]]
      case Classes.FileClass => makeSerializer[Vector[java.io.File]]
      case Classes.BooleanClass => makeSerializer[Vector[Boolean]]
      case Classes.ShortClass => makeSerializer[Vector[Short]]
      case Classes.IntClass => makeSerializer[Vector[Int]]
      case Classes.LongClass => makeSerializer[Vector[Long]]
      case Classes.FloatClass => makeSerializer[Vector[Float]]
      case Classes.DoubleClass => makeSerializer[Vector[Double]]
      case Classes.URIClass => makeSerializer[Vector[java.net.URI]]
      case Classes.ThrowableSubClass() => makeSerializer[Vector[java.lang.Throwable]]
      case _ =>
        None
    }).asInstanceOf[Option[Pickler[Vector[T]] with Unpickler[Vector[T]]]]
  }

  private def defaultSerializerForList[T](klass: Class[T]): Option[Pickler[List[T]] with Unpickler[List[T]]] = {
    (klass match {
      case Classes.StringClass => makeSerializer[List[String]]
      case Classes.FileClass => makeSerializer[List[java.io.File]]
      case Classes.BooleanClass => makeSerializer[List[Boolean]]
      case Classes.ShortClass => makeSerializer[List[Short]]
      case Classes.IntClass => makeSerializer[List[Int]]
      case Classes.LongClass => makeSerializer[List[Long]]
      case Classes.FloatClass => makeSerializer[List[Float]]
      case Classes.DoubleClass => makeSerializer[List[Double]]
      case Classes.URIClass => makeSerializer[List[java.net.URI]]
      case Classes.ThrowableSubClass() => makeSerializer[List[java.lang.Throwable]]
      case _ =>
        None
    }).asInstanceOf[Option[Pickler[List[T]] with Unpickler[List[T]]]]
  }

  private def defaultSerializerForSeq[T](mf: Manifest[T]): Option[Pickler[Seq[T]] with Unpickler[Seq[T]]] = {
    (mf.runtimeClass match {
      case Classes.StringClass => makeSerializer[Seq[String]]
      case Classes.FileClass => makeSerializer[Seq[java.io.File]]
      case Classes.BooleanClass => makeSerializer[Seq[Boolean]]
      case Classes.ShortClass => makeSerializer[Seq[Short]]
      case Classes.IntClass => makeSerializer[Seq[Int]]
      case Classes.LongClass => makeSerializer[Seq[Long]]
      case Classes.FloatClass => makeSerializer[Seq[Float]]
      case Classes.DoubleClass => makeSerializer[Seq[Double]]
      case Classes.URIClass => makeSerializer[Seq[java.net.URI]]
      case Classes.protocol.Attributed => defaultSerializerForSeqAttributed(mf.typeArguments.head.runtimeClass)
      case Classes.ThrowableSubClass() => makeSerializer[Seq[java.lang.Throwable]]
      case _ =>
        None
    }).asInstanceOf[Option[Pickler[Seq[T]] with Unpickler[Seq[T]]]]
  }

  private def defaultSerializerForSeqAttributed[T](klass: Class[T]): Option[Pickler[Seq[Attributed[T]]] with Unpickler[Seq[Attributed[T]]]] = {
    (klass match {
      case Classes.FileClass => makeSerializer[Seq[protocol.Attributed[java.io.File]]]
      case _ =>
        None
    }).asInstanceOf[Option[Pickler[Seq[protocol.Attributed[T]]] with Unpickler[Seq[protocol.Attributed[T]]]]]

  }

  private def defaultSerializer[T](mf: Manifest[T]): Option[Pickler[T] with Unpickler[T]] = defaultSerializationMemosByManifest.get(mf).map(_.asInstanceOf[Pickler[T] with Unpickler[T]]) orElse
    defaultSerializer[T](mf.runtimeClass.asInstanceOf[Class[T]]) orElse {
      // TODO these SubClass tests are sort of BS since picklers are invariant
      (mf.runtimeClass match {
        case Classes.OptionSubClass() =>
          defaultSerializerForOption(mf.typeArguments(0).runtimeClass)

        // Vector and List are special-cased for unpickle; for pickle
        // we could serialize any Seq the same, but on unpickle we want to
        // get the expected type.
        case Classes.VectorSubClass() =>
          defaultSerializerForVector(mf.typeArguments(0).runtimeClass)
        case Classes.ListSubClass() =>
          defaultSerializerForList(mf.typeArguments(0).runtimeClass)
        case Classes.SeqSubClass() =>
          defaultSerializerForSeq(mf.typeArguments(0))

        // case Classes.AttributedSubClass() =>
        //   for {
        //     child <- memoizedDefaultSerializer(mf.typeArguments(0))
        //   } yield ??? /* FIXME */

        case _ =>
          None
      }).asInstanceOf[Option[Pickler[T] with Unpickler[T]]]
    }
}

private object NonTrivialSerializers {
  /* FIXME This appears to lock up compilation forever, so somehow triggers infinite
   * macro loop or something?
   */
  def registerSerializers(base: DynamicSerialization): DynamicSerialization = {
    // TODO I think we only lookup by exact type, so registering an abstract type
    // here such as Type or xsbti.Problem is not useful. I think.
    // TODO we are registering some types here that aren't likely or conceivable
    // task results; we only need to register types T that appear in taskKey[T].
    // We don't have to register all the types of the fields in result types.
    val serializers = Seq[RegisteredSerializer](
      RegisteredSerializer[ProjectReference],
      RegisteredSerializer[AttributeKey],
      RegisteredSerializer[SbtScope],
      RegisteredSerializer[ScopedKey],
      RegisteredSerializer[MinimalBuildStructure],
      RegisteredSerializer[CompileFailedException],
      RegisteredSerializer[ModuleId],
      RegisteredSerializer[protocol.Attributed[File]])
    serializers.foldLeft(base) { (sofar, next) =>
      sofar.register(next.serializer)(next.manifest)
    }
  }
}
