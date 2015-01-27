package sbt.serialization
package pickler

trait SerializationPicklers {

  implicit val serializedValuePickler = SerializedValue.pickler
}
