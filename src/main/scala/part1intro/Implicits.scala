package part1intro

object Implicits extends App{

  // Implicit classes
  case class Person(name: String){
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String){
    def greet : String = Person(name).greet
  }

  val impersonableString = new ImpersonableString("Peter")
  impersonableString.greet

  val greeting = "Peter".greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.seconds
  println(oneSec.getClass)

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x  + amount
  implicit val defaultAmount = 10
  val incremented2 = increment(9) // 10 is passed via compiler
  println(incremented2)

  def multiply(x: Int)(implicit muliplicant: Int) = x * muliplicant
  val times2 = multiply(2)
  println(times2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[A](lst: List[A])(implicit s  :JSONSerializer[A]) : String = {
      lst.map(s.toJson).mkString("[",",","]")
  }

  implicit val personSerializer = new JSONSerializer[Person] {
    override def toJson(value: Person): String = s"""{"name": ${value.name}}""".stripMargin
  }
  val explicitSerializer = new JSONSerializer[Person] {
    override def toJson(value: Person): String = value.toString
  }

  val personsJson = listToJson(List(Person("Alice"), Person("Bob"))) // (explicitSerializer
  println(personsJson)

  // Implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product] : JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""{"${value.productElementName(0)}" : "${value.productElement(0)}"}""".stripMargin
  }

  case class Cat(kittyname: String)
  println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
  println(oneArgCaseClassSerializer[Person].toJson(Person("Robertr")))

  val catsToJson = listToJson(List(Cat("Tom"),Cat("Garfield"),Person("Robert")))
  println(catsToJson)

  // implicits can also be used as a conversion, but that's discouraged because its confusing


}
