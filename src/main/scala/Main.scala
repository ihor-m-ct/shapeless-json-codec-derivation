import json.encoder.ToJsonObject
import shapeless.{Generic, HList, the}

object Main extends App {

  case class Example1(name: String, age: Int, isBodyPositive: Boolean)
  case class Example2(test: String, list: List[Int] = Nil)

  val example1 = Example1("Punto", 25, isBodyPositive = true)
  val example2 = Example2("test")
  val genericTuple = implicitly[Generic[Example1]].to(example1)
  val tuple1 = ("Punto", 25, true)
  val genericTuple2 = implicitly[Generic[(String, Int, Boolean)]].to(tuple1)
  val genericTuple3 = HList(tuple1)

  val encodedExample1 = ToJsonObject.encode(example1)
  println(encodedExample1)

  val example2Encoder: ToJsonObject[Example2] = the[ToJsonObject[Example2]]
  println(example2Encoder.encode(example2))

}
