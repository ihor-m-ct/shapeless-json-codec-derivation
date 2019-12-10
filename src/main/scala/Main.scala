import json.encoder.{ToJson, ToJsonObject}
import shapeless.{Generic, HList, the}

object Main extends App {

  case class Example1(name: String, age: Int, isBodyPositive: Boolean)
  case class Example2(test: String, list: List[Int] = Nil, optionalField: Option[String] = None, example1: Option[Example1] = None)

  val example1 = Example1("Punto", 25, isBodyPositive = true)
  val example2 = Example2("test", List(1, 2, 3), Some("option"), Some(example1))
  val genericTuple = the[Generic[Example1]].to(example1)
  val tuple1 = ("Punto", 25, true)
  val genericTuple2 = the[Generic[(String, Int, Boolean)]].to(tuple1)
  val genericTuple3 = HList(tuple1)

  val encodedExample1 = ToJsonObject.encode(example1)
  println(encodedExample1)

  // TODO - remove it and compile
  import ToJsonObject.encodeAsLabelledGeneric

  val example2Encoder: ToJsonObject[Example2] = the[ToJsonObject[Example2]]

  println(example2Encoder.encode(example2))

}
