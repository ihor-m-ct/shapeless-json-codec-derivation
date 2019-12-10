package json.encoder

import json._
import shapeless.{Lazy, the}

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

object ToJson extends LowPriorityToJsonImplicits {
  def encode[T: ToJson](value: T): Json = the[ToJson[T]].encode(value)
}
