package json.encoder

import json.{Json, JsonObject}
import shapeless.{::, HList, HNil, HasProductGeneric, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType

trait ToJsonObject[-T] extends ToJson[T] {
  def encode(value: T): JsonObject
}

trait LowPriorityLabelledGenericToJson extends LowPriorityToJsonImplicits {

  @inline
  implicit def encodeAsLabelledGeneric[X : HasProductGeneric, H <: HList](
                                                                           implicit gen: LabelledGeneric.Aux[X, H],
                                                                           encoder: ToJsonObject[H]): ToJsonObject[X] =
    (value: X) => encoder.encode(gen.to(value))

}

trait MidPriorityLabelledGenericToJson extends LowPriorityLabelledGenericToJson {

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

}

object ToJsonObject extends MidPriorityLabelledGenericToJson {

  def encode[X, H <: HList](value: X)(implicit gen: LabelledGeneric.Aux[X, H],
                                      encoder: ToJsonObject[H]): Json = {
    encoder.encode(gen.to(value))
  }

}
