package part1intro

object TypeClasses extends App {

  case class Person( name: String, age: Int)
  // part 1 of type classes:  type class definition
  trait JSONSerializer[T] {
    def toJson(value: T) : String
  }

  // part 2 : create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String = s"""{ "name" : "${value.name}", "age" : ${value.age}}"""
  }

  // part 3 : offer an API to convert to json
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]) : String = {
    list.map(v => serializer.toJson(v) ).mkString("[",",","]")
  }
  println(convertListToJSON(List("alpha", "beta", "gamma")))
  println(convertListToJSON(List(Person("Mia", 16), Person("Christine",16))))

  // Part 4 - Extend the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value: T)
    }
  }

  // and if you import the above class, you get the "toJson" extension method
  import JSONSyntax._
  println( Person("Bob", 35).toJson )

}
