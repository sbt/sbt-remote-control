package sbt.serialization

import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }
import sbt.serialization.pickler.{
  PrimitivePicklers,
  PrimitiveArrayPicklers,
  OptionPicklers,
  RefPicklers,
  VectorPicklers,
  ListPicklers,
  ArrayPicklers,
  SeqPicklers,
  StringMapPicklers,
  CanToStringPicklers
}

trait CustomPicklerUnpickler extends PrimitivePicklers
  with PrimitiveArrayPicklers
  with OptionPicklers
  with CanToStringPicklers
  with RefPicklers
  with LowPriorityCustomPicklerUnpickler {}

trait LowPriorityCustomPicklerUnpickler extends VectorPicklers
  with ListPicklers
  with ArrayPicklers
  with SeqPicklers
  with StringMapPicklers {}
