package json.encoder

import json._
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, HasProductGeneric, LabelledGeneric, Lazy, Witness, the}

import scala.language.implicitConversions

trait ToJson[-T] {
  def encode(value: T): Json
}

trait LowPriorityToJsonImplicits {
  implicit val stringToJson: ToJson[String] = (value: String) =>
    JsonString(value)

  implicit val intToJson: ToJson[Int] = (value: Int) =>
    JsonNumber(BigDecimal(value))

  implicit val doubleToJson: ToJson[Double] = (value: Double) =>
    JsonNumber(BigDecimal(value))

  implicit val boolToJson: ToJson[Boolean] = (bool: Boolean) =>
    JsonBoolean(bool)

  implicit def optionToJson[T](implicit toJson: Lazy[ToJson[T]]): ToJson[Option[T]] = {
    case Some(value) => toJson.value.encode(value)
    case None        => JsonNull
  }

  implicit def listToJson[T](implicit toJson: Lazy[ToJson[T]]): ToJson[List[T]] =
    (value: List[T]) => JsonArray(value.map(v => toJson.value.encode(v)).toVector)
}

trait MediumPriorityLabelledGenericToJson extends LowPriorityToJsonImplicits {
  implicit val encodeObjectHNil: ToJson[HNil] = (_: HNil) =>
    JsonObject(Map.empty)

  @inline
  implicit def encodeObjectHCons[HK <: Symbol, HV, T <: HList](
                                                                implicit headEncoder: Lazy[ToJson[HV]],
                                                                tailEncoder: ToJson[T],
                                                                headKey: Witness.Aux[HK]
                                                              ): ToJson[FieldType[HK, HV] :: T] =
    (value: FieldType[HK, HV] :: T) => {
      val encodedHeadKey = headKey.value.name
      val encodedHeadValue = headEncoder.value.encode(value.head)
      val encodedTail = tailEncoder.encode(value.tail)
      val values: Map[String, Json] =
        encodedTail match {
          case JsonObject(values) => values
          case _ => Map.empty
        }

      JsonObject(values + (encodedHeadKey -> encodedHeadValue))

    }
}

object ToJson extends MediumPriorityLabelledGenericToJson {

  implicit def encodeAsLabelledGeneric[X : HasProductGeneric, H <: HList](
                                                                           implicit labelledGeneric: LabelledGeneric.Aux[X, H],
                                                                           productEncoder: ToJson[H]): ToJson[X] =
    (value: X) => productEncoder.encode(labelledGeneric.to(value))

  def encode[T: ToJson](value: T): Json = the[ToJson[T]].encode(value)
}
