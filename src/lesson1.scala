val normalList = 1 :: "SomeString" :: true :: Nil

import json.encoder.ToJsonObject
import shapeless._
import shapeless.labelled.FieldType
val hList = 1 :: "SomeString" :: true :: HNil

object size extends Poly1 {
  implicit def default[T] = at[T](t => 1)
  implicit def caseString = at[String](_.length)
  implicit def caseList[T] = at[List[T]](_.length)
}

val sizes = hList.map(size)
sizes.toList


sealed trait Json
case class JsonString(value: String) extends Json
case class JsonNumber(value: BigDecimal) extends Json
case class JsonBoolean(value: Boolean) extends Json
case class JsonArray(value: Vector[Json]) extends Json
case class JsonObject(value: Map[String, Json]) extends Json
case object JsonNull extends Json

object encoders {
  trait ToJson[T] {
    def encode(value: T): Json
  }

  trait LowPriorityToJsonImplicits {
    implicit val stringToJson: ToJson[String] = (value: String) => JsonString(value)
    implicit val intToJson: ToJson[Int] = (value: Int) => JsonNumber(BigDecimal(value))
    implicit val doubleToJson: ToJson[Double] = (value: Double) => JsonNumber(BigDecimal(value))
    implicit val boolToJson: ToJson[Boolean] = (bool: Boolean) => JsonBoolean(bool)
    implicit def optionToJson[T : ToJson](opt: Option[T]): ToJson[Option[T]] = {
      case Some(value) => the[ToJson[T]].encode(value)
      case None => JsonNull
    }
    implicit val listToJson: ToJson[List[Json]] = (list: List[Json]) => JsonArray(Vector(list:_*))
  }

  object ToJson extends LowPriorityToJsonImplicits {

  }
}

import encoders._


def encode[T : ToJson](value: T): Json = implicitly[ToJson[T]].encode(value)


val jsString1 = encode("String")


case class Example1(name: String, age: Int, isBodyPositive: Boolean)

val example1 = Example1("Punto", 25, isBodyPositive = true)
val genericTuple = implicitly[Generic[Example1]].to(example1)
val tuple1 = ("Punto", 25, true)
val genericTuple = implicitly[Generic[(String, Int, Boolean)]].to(tuple1)
val genericTuple2 = HList(tuple1)

object HListToJson {

  implicit val encodeHNil: ToJson[HNil] = (_: HNil) => JsonArray(Vector.empty)

  @inline
  implicit def encodeHCons[H, T <: HList](
                                          implicit headEncode: ToJson[H],
                                          tailEncode: ToJson[T]
                                         ): ToJson[H :: T] = (value: H :: T) => {
    val encodedHead: Json = headEncode.encode(value.head)
    tailEncode.encode(value.tail) match {
      case JsonArray(existingValues: Vector[Json]) =>
        JsonArray(existingValues.prepended(encodedHead))
      case _ =>
        JsonArray(Vector(encodedHead))
    }
  }
}

import HListToJson._

def encodeHList[T <: HList](value: T)(implicit encoder: ToJson[T]): Json =
  encoder.encode(value)

val encodedHList = encodeHList(hList)

def encodeGeneric[X, H <: HList](value: X)
                                (implicit gen: Generic.Aux[X, H],
                                 encoder: ToJson[H]): Json = {
  val repr = gen.to(value)
  encoder.encode(repr)
}

val encodedGenericExample = encodeGeneric(Example1("Punto", 25, isBodyPositive = true))

encodeGeneric((1, "String"))


trait ToJsonObject[T] extends ToJson[T] {
  def encode(value: T): JsonObject
}

object LabelledGenericToJson {
  implicit val encodeObjectAsHNil: ToJsonObject[HNil] = (_: HNil) => JsonObject(Map.empty)

  @inline
  implicit def encodeObjectHCons[HK <: Symbol, HV, T <: HList](
                                                     implicit headEncoder: ToJson[HV],
                                                     tailEncoder: ToJsonObject[T],
                                                     headKey: Witness.Aux[HK]
                                                    ): ToJsonObject[FieldType[HK, HV] :: T] =
    (value: FieldType[HK, HV] :: T) => {
      val encodedHeadKey = headKey.value.name
      val encodedHeadValue = headEncoder.encode(value.head)
      val encodedTail = tailEncoder.encode(value.tail)
      JsonObject(encodedTail.value + (encodedHeadKey -> encodedHeadValue))
    }
}

def encodeLabelledGeneric[X, H <: HList](value: X)
                                        (implicit gen: LabelledGeneric.Aux[X, H],
                                         encoder: ToJsonObject[H]): Json = {
  encoder.encode(gen.to(value))
}

import LabelledGenericToJson._

encodeLabelledGeneric(example1)
