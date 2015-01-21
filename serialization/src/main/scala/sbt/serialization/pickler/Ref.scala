package sbt.serialization
package pickler

import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }
import scala.pickling.refs.Ref

trait RefPicklers {
  implicit def refPickler: SPickler[Ref] = throw new Error("cannot pickle refs") // TODO: make this a macro
  implicit val refUnpickler: Unpickler[Ref] = PrimitivePickler[Ref]
}
