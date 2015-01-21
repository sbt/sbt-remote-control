package sbt.serialization

import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }
import sbt.serialization.pickler.{
  PrimitivePicklers,
  PrimitiveArrayPicklers,
  OptionPicklers,
  ThrowablePicklers,
  RefPicklers,
  VectorPicklers,
  ListPicklers,
  ArrayPicklers,
  SeqPicklers,
  StringMapPicklers,
  CanToStringPicklers
}

trait CustomPicklers extends PrimitivePicklers
  with PrimitiveArrayPicklers
  with OptionPicklers
  with ThrowablePicklers
  with CanToStringPicklers
  with RefPicklers
  with LowPriorityCustomPicklers {}

trait LowPriorityCustomPicklers extends VectorPicklers
  with ListPicklers
  with ArrayPicklers
  with SeqPicklers
  with StringMapPicklers {}
