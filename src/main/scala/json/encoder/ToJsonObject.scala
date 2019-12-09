package json.encoder

import json.{Json, JsonObject}
import shapeless.{
  ::,
  HList,
  HNil,
  LabelledGeneric,
  Lazy,
  Witness
}
import shapeless.labelled.FieldType

trait ToJsonObject[-T] extends ToJson[T] {
  def encode(value: T): JsonObject
}

trait LowPriorityLabelledGenericToJson {
  implicit val encodeObjectHNil: ToJsonObject[HNil] = (_: HNil) =>
    JsonObject(Map.empty)

  @inline
  implicit def encodeObjectHCons[HK <: Symbol, HV, T <: HList](
      implicit headEncoder: Lazy[ToJson[HV]],
      tailEncoder: ToJsonObject[T],
      headKey: Witness.Aux[HK]
  ): ToJsonObject[FieldType[HK, HV] :: T] =
    (value: FieldType[HK, HV] :: T) => {
      val encodedHeadKey = headKey.value.name
      val encodedHeadValue = headEncoder.value.encode(value.head)
      val encodedTail = tailEncoder.encode(value.tail)
      JsonObject(encodedTail.value + (encodedHeadKey -> encodedHeadValue))
    }

  implicit def encodeAsLabelledGeneric[X, H <: HList](
      implicit gen: LabelledGeneric.Aux[X, H],
      encoder: ToJsonObject[H]): ToJsonObject[X] =
    (value: X) => encoder.encode(gen.to(value))
}

object ToJsonObject extends LowPriorityLabelledGenericToJson {

  def encode[X, H <: HList](value: X)(implicit gen: LabelledGeneric.Aux[X, H],
                                      encoder: ToJsonObject[H]): Json = {
    encoder.encode(gen.to(value))
  }

}
