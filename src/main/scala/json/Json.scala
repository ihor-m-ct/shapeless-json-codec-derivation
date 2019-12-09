package json

sealed trait Json
case class JsonString(value: String) extends Json
case class JsonNumber(value: BigDecimal) extends Json
case class JsonBoolean(value: Boolean) extends Json
case class JsonArray(value: Vector[Json]) extends Json
case class JsonObject(value: Map[String, Json]) extends Json
case object JsonNull extends Json
