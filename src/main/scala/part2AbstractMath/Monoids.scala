package part2AbstractMath

object Monoids extends App{

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.monoid._

  val numbers = (1 to 1000).toList
  // |=| is always associative:
  val numLeft = numbers.foldLeft(0)(_ |+| _)
  val numRight = numbers.foldRight(0)(_ |+| _)
  println(s"because combine is associative, $numLeft should equal $numRight")

  // define a general API
  def combineFoldBad[T](list: List[T])(implicit semigroup: Semigroup[T]) : T
  = list.foldRight(list.head)(_ |+| _)
  println(s"but combine fold is ${combineFoldBad(numbers)} because theres no way to get the starting value! (we are double counting the first elem by using the head)")

  /// MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(3, 4) // same as semigroup
  val zero = intMonoid.empty // intuitive starting position

  // TODO 1: combine Fold
  def monoidFold[T](list: List[T])(implicit monoid: Monoid[T]) : T =
    list.foldRight(monoid.empty)(_ |+| _)
  println(s"...but if you use a monoid it should be correct: ${monoidFold(numbers)}")

  import cats.instances.option._
  val emptyOption = Monoid[Option[Int]].empty
  val combineOption =  Monoid[Option[Int]].combine(Option(3), Option.empty[Int])
  println(s"Empty is $emptyOption and combine is $combineOption")

  // EXTENSION METHODS
  // bar add ( |+| ) which we saw above

  // TODO 2: Combine a list of phonebooks as Maps[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map._
  val wholePhonebook = monoidFold(phonebooks)
  println(s"The whole stinkin book is ${wholePhonebook}")

  // TODO 3: Shopping carts with monoids
  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid : Monoid[ShoppingCart] = Monoid.instance[ShoppingCart] (
    ShoppingCart(List.empty, 0),
     (sc1, sc2) => ShoppingCart(sc1.items ++ sc2.items, sc1.total + sc2.total)
  )
  def checkout( shoppingCarts: List[ShoppingCart]) : ShoppingCart = monoidFold(shoppingCarts)

  println(checkout(List(
    ShoppingCart(List("iphone", "shoes"), 799),
    ShoppingCart(List("tv"), 2000),
    ShoppingCart(List(), 0)
  )))

}
