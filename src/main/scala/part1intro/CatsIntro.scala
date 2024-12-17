package part1intro

object CatsIntro extends App {

  // Eq
  //val aComparison = 2 == "a string" // always false because different types

  // part 1 - type class import
  import cats.Eq

  // part 2 - import TC instances for the types you need
  import cats.instances.int._

  // part 3 - use TC Api
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
  //val aTypeUnsafeEquality = intEquality.eqv(2, "two") // does not compile because different types

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComparison = 2 === 3 // false
  val neqComparison = 2 =!= 3 // true
  println(s"equal? $anotherTypeSafeComparison neq? $neqComparison")
  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e.g. lists
  import cats.instances.list._ // we bring Eq[List] in scope.  List[Int] works because of the import in part 2
  val aListComparison = List(2) === List(3)
  println(s"The lists are equal? $aListComparison")

  // part 6  - What if our type is not supported in Cats?
  // create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val tcTonka = ToyCar("Tonka", 25)
  val tcMatchbox = ToyCar("Matchbox", 5)
  val tcSpeedo = ToyCar("Speedo", 5)

  println(s"Matchbox==Speedo? ${tcMatchbox === tcSpeedo}:  Matchbox==Tonka? ${tcMatchbox === tcTonka}")

}
