package part4TypeClasses

import cats.{Applicative, Foldable, Monad}

import java.util.concurrent.Executors
import scala.:+
import scala.concurrent.{ExecutionContext, Future}

object Traversing extends App {
  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers : List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  def getBandwidth(hostname: String) : Future[Int] = Future{ hostname.length * 88}

  /*
  we have
    - a List[String]
    - a func String => Future[Int]
    we want a Future[List[Int]]
   */
  val allBandwidths : Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])){
    (accumulator, hostname) =>
      val bandFuture = getBandwidth(hostname)
      for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsBetter: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwithsSequence :Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // Traverse w a option (a little stupid, but) doesn't work
 //val maybeServer = Some("robserver")
  //val allOneBandwidth = Future.traverse(maybeServer)(getBandwidth)

  // TODO
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatmap
  import cats.syntax.functor._ // map
  def listTraverseFake[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accumulator, element) =>
        val wrapped = func(element)
        for {
          elems <- accumulator
          el <- wrapped
        } yield elems :+ el
    }

  import cats.syntax.apply._ // mapn

  def listTraverseReal[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accumulator, element) =>
        val wrapped = func(element)
        (accumulator, wrapped).mapN(_ :+ _)
    }

  // TODO 2
  def listSequence[F[_]:Applicative, A](list: List[F[A]]) : F[List[A]] =
    listTraverseReal(list)(x=>x)
//    list.foldLeft(List.empty[A].pure[F])
//    {
//      (accumulator, element) =>
//        (accumulator, element).mapN(_ :+ _)
//    }

  // TODO 3  what do these return?
  import cats.instances.vector._
  val seq1 = listSequence(List(Vector(1,2), Vector(3,4)))
  val seq2 = listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))
  println(seq1) // all pairs
  println(seq2) // all pairs (3)

  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean) : Option[List[Int]] =
    listTraverseReal[Option, Int, Int](list)(n => Some(n).filter(predicate))

  // whats the result of...
  val allOfEm = filterAsOption(List(2,4,6))(_ % 2 == 0) // Some(List(2, 4, 6))
  println(allOfEm)
  val noneOfEm = filterAsOption(List(1,2,3))(_ % 2 == 0) // Some (2)? NO none! Because Some combined with None is None
  println(noneOfEm)

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List]
  type ErrorsOr[T]  =Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseReal[ErrorsOr, Int, Int](list){
      n => if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"Ooops!$n"))
    }

  val listOValids = filterAsValidated(List(2,4,6))(_ % 2 == 0)
  println(listOValids)
  val listOMixes = filterAsValidated(List(1, 2,5,4,6))(_ % 2 == 0)
  println(listOMixes) // just invalids

  trait MyTraverse[L[_]] extends Foldable[L] {
    def traverse[F[_]: Applicative, A, B](list: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_] :Applicative, A]  (list: L[F[A]]) : F[L[A]] = traverse(list)(identity)

    // TODO 6 implement w. traverse and or sequence
    //type Identity[T] = T
    import cats.Id //built in identity which provides instances included Applicative which is needed below
    def map[A, B](wa: L[A])(f: A => B) : L[B] = {
      traverse[Id, A, B](wa)(f)
      //traverse[Identity, A, B](wa)(f)a
    }
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._
  val allBandwiodthsWithCVats2 = servers.traverse(getBandwidth)

}
