package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals extends App {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._  // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupleOption = optionSemigroupal.product(Some(1,2,3), Some("a string"))
  println(aTupleOption)  //Some(((1,2,3),a string))
  val aNoneTuple = optionSemigroupal.product(Some("junko"), None)
  println(aNoneTuple)  //None

  import cats.instances.future._
  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future{ 450}, Future { "sometime en futero"})
  println(aTupledFuture)
  aTupledFuture.map(println)

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1,2,3), List('a', 'b', 'c', 'd'))
  println(aTupledList) // cartesian product

  // TODO: implement products with monads
  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]) : F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // MONADS EXTEND SEMIGROUPALS

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]

  val invalidsCompination = validatedSemigroupal.product(
    Validated.invalid(List("Something Wrong", "something else wong")),
    Validated.invalid(List("this cannot be right"))
  )
  println(invalidsCompination)

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eithersCombination = eitherSemigroupal.product(  // in terms of map/flatMap
    Left(List("Something Wrong", "something else wong")),
    Left(List("this cannot be right")) // not propigated because the mondad flat map short circuits the propigation
  )
  println(eithersCombination)

  // TODO 2:  define a Semigroupal[List] which does a zip
//  class semigroupalWithList[F[_]] extends Semigroupal[List] {
//    override def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)] = {
//
//    }
//  }

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  println(s"Zipped:  ${zipListSemigroupal.product(List("Me","You","Other"),List(71,72))}")


}
