package part1intro

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions are EVAUATED to a value
  val anIfExpression: String = if (2 > 3) "bigger" else "smaller"

  // Instructions vs expressions
  val theUnit: Unit = println("Hello, scala") // unit = void in other languages

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model:  Extend <=1 class but inherit from >= traits
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("gulp!")
  }

  // singleton
  object mySingleton

  // companions (same name as class) are like static methods in Java
  object Carnivore

  // generics
  class MyList[A]

  // method notation
  val three: Int = 1 + 2
  val alsoThree: Int = 1.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented: Int = incrementer(45)

  // higher order functions are any functions which take or return another function
  // example: map, flatMap, filter
  val processedList: List[Int] = List(1,2,3).map(incrementer)
  val aLongerList: List[Int] = List(1,2,3).flatMap(x => List(x, x +  1))

  // for comprehensions
  val checkerboard: Seq[(Int, Char)] = List(1,2,3).flatMap(n => List('a', 'b', 'c').map(c=> (n, c)))
  val checkerboard2 = for {
    n <- List(1,2,3)
    c <- List('a', 'b', 'c')
  } yield (n,c)

  // Options and Try
  val anOption: Option[Int] = Option(/*something that may be null*/ 3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2)

  val anAttempt: Try[Int] = Try( /* something which might throe */ 42) // Success(40) or Failure
  val aModifiedAttempt : Try[Int] = anAttempt.map(_ + 10)

  // pattern matching
  val anUnknown : Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the option is not empty $value"
    case None => "the option is empty"
  }

  // Futures
  // import scala.concurrent.ExecutionContext.Implicits.global // up to 2.12.
  // In 2.13, nested futures will NOT run in paralell unless we use the blocking api
  // This is poor design (in the opinion of the instructor) so we will roll our own context instead!
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture: Future[Int] = Future {
    // a bit of code
    42
  }

  // wait for completion (async)
  // on complete receives a Try
  // this block is a "partial function
  aFuture.onComplete{
    case Success(value) => println(s"The meaning of life is $value")
    case Failure(exception) => println(s"There is no meaning of life because $exception")
  }

  // map a future
  val anotherFuture: Future[Int] = aFuture.map(_ + 1)

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 8 => 56
    case 180 => 999
  }

  // some more advanced stuff
  trait HigherKindedType[F[_]]  // Trait with a type variable which ITSELF might have a type variable
  trait SequenceChecker[A[_]] {
    def isSequential : Boolean
  }

  val listChecker: SequenceChecker[List] = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }


  def main(args: Array[String]) :Unit = {
    val lOO : List[Option[Int]] = List(Some(10), None, Some(30), None, None, Some(60))
    // map operates on an options and returns None if theres no element
    // the outer map will return the original type (Option[Int]) ...
    println( lOO.map( _.map(_ * 2)))
    // ... whereas flatMap will "unoption" the elements
    println( lOO.flatMap( _.map(_ * 2)))
    // ... only need to flatten the outer in this case
    //println( lOO.flatMap( _.flatMap(_ * 2)))

  }
}
