package part2AbstractMath

object Semigroups extends App {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition?
  println(s"intCombination is ${intCombination}") // yes!

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("hi ", "there") // concat??
  println(s"stringCombination is $stringCombination")

  import cats.instances.list._
  val naturalListSemigroup = Semigroup[List[Any]]
  val listAnyCombination = naturalListSemigroup.combine(List("a","b","c"), List(1, 2, 3))
  println (s"listAnyCombination is $listAnyCombination")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]) : String = list.reduce(naturalStringSemigroup.combine)

  println(s"a reduction of digits is ${reduceInts(List(1,2,3,4,5,6,7,8,9,0))}")
  println(s"a reduction of words is '${reduceStrings(List("every"," good", " boy", " deserves", " fudge"))}''")

  // general Api
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]) : T = list.reduce(semigroup.combine)

  println(s"a generic reduction of digits is ${reduceThings(List(1,2,3,4,5,6,7,8,9,0))}")
  println(s"a generic reduction of words is '${reduceThings(List("every"," good", " boy", " deserves", " fudge"))}'")

  // bring into scope the options instance
  import cats.instances.option._
  // and use generic without creating a Semigroup explicitly
  println(s"a generic reduction of Option[Int] (with a getOrElse) is ${reduceThings[Option[Int]](List(Some(1), None, Some(2), Some(3))).getOrElse(0)}")
  // below wont work because there is a not a Long instance in scope
  //println(s"a generic reduction of Option[Long] (with a getOrElse) is ${reduceThings[Option[Long]](List(Some(1), None, Some(2), Some(3))).getOrElse(0)}")

  // TODO 1: support a new type
  case class Expense(id: Long, amount: Double)
  implicit val sgExpense : Semigroup[Expense] = Semigroup.instance[Expense] {
    (grp1, grp2) => Expense( Math.max(grp1.id, grp2.id), grp1.amount + grp2.amount)
  }
  println(s"a generic reduction of Expense objects is ${
    reduceThings(List(Expense(1, 10000), Expense(7, 7878), Expense(3,800000)))
  }")

  // EXTENSION METHODS
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 4 // need an implicit semigroup in scope (int for this one)

  // TODO 2: Implement reduceThings2
  def reduceThings2[T: Semigroup](list: List[T]) : T = list.reduce(_ |+| _ )

   println(s"a different generic reduction of expenses is (also) ${
     reduceThings2(List(Expense(1, 10000), Expense(7, 20000), Expense(3,5000)))
   }")


}

