// -- Page 3 --
import shapeless.{nat, _}
import shapeless.labelled.FieldType
import shapeless.ops.nat.LT

import scala.language.implicitConversions

val normalList = 1 :: "SomeString" :: true :: Nil
val hList = 1 :: "SomeString" :: true :: HNil

// -- Page 4 --

object size extends Poly1 {
  implicit def default[T] = at[T](t => 1)
  implicit def caseString = at[String](_.length)
  implicit def caseList[T] = at[List[T]](_.length)
}

val sizes = hList.map(size)
sizes.toList

// -- Page 5 --

sealed trait Json
case class JsonString(value: String) extends Json
case class JsonNumber(value: BigDecimal) extends Json
case class JsonBoolean(value: Boolean) extends Json
case class JsonArray(value: Vector[Json]) extends Json
case class JsonObject(value: Map[String, Json]) extends Json
case object JsonNull extends Json

// -- Page 6 --

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

    implicit def listToJson[T : ToJson]: ToJson[List[T]] =
      (list: List[T]) => JsonArray(list.map(value => the[ToJson[T]].encode(value)).toVector)
  }

  object ToJson extends LowPriorityToJsonImplicits
}

import encoders._

def encode[T : ToJson](value: T): Json = implicitly[ToJson[T]].encode(value)


val jsString1 = encode("String")

// -- Page 7 --

case class Example1(name: String, age: Int, isBodyPositive: Boolean)

val example1 = Example1("Punto", 25, isBodyPositive = true)
val genericTuple = implicitly[Generic[Example1]].to(example1)
val tuple1 = ("Punto", 25, true)
val genericTuple = implicitly[Generic[(String, Int, Boolean)]].to(tuple1)
val genericTuple2 = HList(tuple1)


// -- Page 8 --

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


// -- Page 9 --


val testTuple: (String, Int, Boolean) = ("Hello", 1, true)
val hlistFromTuple: String :: Int :: Boolean :: HNil = HList(testTuple)
val jsonFromHList: ToJson[String :: Int :: Boolean :: HNil] =
  encodeHCons[String, Int :: Boolean :: HNil](
    ToJson.stringToJson,
    encodeHCons[Int, Boolean :: HNil](
      ToJson.intToJson,
      encodeHCons[Boolean, HNil](
        ToJson.boolToJson,
        encodeHNil
      )
    )
  )
val encodingResult = jsonFromHList.encode(hlistFromTuple)

// -- Page 10 --

def encodeGeneric[X, H <: HList](value: X)
                                (implicit gen: Generic.Aux[X, H],
                                 encoder: ToJson[H]): Json = {
  val repr = gen.to(value)
  encoder.encode(repr)
}

val encodedGenericExample = encodeGeneric(Example1("Punto", 25, isBodyPositive = true))

encodeGeneric((1, "String"))

// -- Page 11 --

trait MyDependantType {
  type X
}

object MyDependantType {
  type Aux[T] = MyDependantType {
    type X = T
  }
}

// -- Page 12 --

val l = 'hello
val l1 = Symbol("hello")

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

//encodeLabelledGeneric(example1)

import shapeless.ops.nat._
import shapeless.nat._

val threeElements = Sized(1,2,3)
val oneElement = Sized(1)
val fourElements = Sized(5,6,7,8)

def sumFirstAndSecond[L <: Nat](sized: Sized[IndexedSeq[Int], L])
                               (implicit diff1: Diff[L, Succ[_0]],
                                diff2: Diff[L, Succ[_1]]): Int = {
  sized.at[_0] + sized.at[_1]
}

val literal: 42 = 42


sumFirstAndSecond(threeElements)
sumFirstAndSecond(oneElement)
sumFirstAndSecond(fourElements)
