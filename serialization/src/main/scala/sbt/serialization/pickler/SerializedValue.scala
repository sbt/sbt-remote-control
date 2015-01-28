package sbt.serialization
package pickler

trait SerializationPicklers {
  implicit val serializedValuePickler: SPickler[SerializedValue] with Unpickler[SerializedValue] = SerializedValue.pickler
}
