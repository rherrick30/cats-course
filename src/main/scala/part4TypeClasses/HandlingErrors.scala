package part4TypeClasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors extends App {

  trait MyApplicativeError[M[_], E] extends Applicative[M]{
    // note that E is ANY kind of error that matters to you
    // does not have to be Exception derived
    def raiseError[A](e: E) : M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e=> pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends  MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(func : A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  // note the second type below must match the error type
  val monadErrorEither = MonadError[ErrorOr, String]

  // because the monand error IS a monad you have access to monad methods...
  val success : Either[String, Int] = monadErrorEither.pure(32) // Right(32)
  println(success)

  // .. plus the raise error
  // note supply the Right type in case its composed
  val failure = monadErrorEither.raiseError[Int]("Baaaaad") // Left(Baaaaad)
  println(failure)

  // the handleError convenience method allows you to recover from an error
  val handledError : ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }

  // the handleErrorWith convenience method is like handleWith except it returns another wrapper type
  val handedErrors2 : ErrorOr[Int] = monadErrorEither.handleErrorWith(failure){
    case "Badness" => failure
    case "Bssssss" => monadErrorEither.pure(88)
    case "Grrrr" => Left("Hmmm")
    case _ => monadErrorEither.raiseError("Gee something happend and we are not sure...")
  }

  // ensure(wrapper)(newFail)(pred)->   return newFail if pred.apply(wrapper)
  // often this is used as an extension method because it is soo clunky!
  val filteredSucess  = monadErrorEither.ensure(success)("Number to small")(_ > 100)

  // Try and future
  import cats.instances.try_._ // import implicit MonadError[Try] = E = Throwable
  val exception = new RuntimeException("really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure exception

  import cats.instances.future._
  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureException = MonadError[Future, Throwable].raiseError(exception) // future which will complete with a Failure(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // applicative error has the same Api as MonadError: pure, raiseError, handleError, handleErrorWith

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError(With)
  val extendedSuccess = 42.pure[ErrorsOr] // requires implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError = extendedError.recover{
    case _ => 43
  }

  import cats.syntax.monadError._
  val monadErroric = success.ensure("Something Bad")(_ > 100)


}
