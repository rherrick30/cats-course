package part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App {

  def sumAllOptions(values: List[Option[Int]]) : Int = ???

  import cats.data.OptionT
  import cats.instances.list._ // fetches an implicit OptionT[List]
  import cats.instances.future._  // I FORGOT TO DO THIS INITIALLY!!!

  // OptionT[List, Int] is a List of Option[Int]
  val listNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listCharOptions: OptionT[List, Char] = OptionT(List(Some('a'), Some('b'), None))
  val listOfTuples : OptionT[List, (Int, Char)] = for {
    char <- listCharOptions
    number <- listNumberOptions
  } yield (number, char)
  println(s"List of tuples is ${listOfTuples.value}")


  // Either transformer
  import cats.data.EitherT
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))
  val listOfEithers : EitherT[List, String, Int] = EitherT(List(Left("what's wrong?"),Right(18), Right(22)))
  //val futureOfEither: EitherT[Future, String, Int] = EitherT(Future[Either[String,Int]](Right(45)))
  // note above the explicit typing in the type due to variance between Either and Right.  The below shows how to do wo/ that boilerplate
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))
  /*
    Assume we have a multi-machine cluster for your business which will get a surge following a media appearance
    We measure bandwidth in units
    We want to allocate TWO of our servers to cope with the traffic spike
    We know the current capacity for each server and we know we'll hold traffic if the sum of bandwidths > 250
   */

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]
  def getBandwidth(server: String) : AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"server ${server} unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO
  def canWithstandSurge(s1: String, s2: String) : AsyncResponse[Boolean] =
    for {
      s10K <- getBandwidth(s1)
      s20K <- getBandwidth(s2)
    } yield s10K + s20K > 250


  def generateTrafficSpikeReport(s1: String, s2: String) : AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Right(true) => Right(s"Can do it with $s1 and $s2")
      case Right(false) => Left(s"Cannot do it with $s1 and $s2 due to low bandwidth")
      case Left(err) => Left(s"Cannot do it with $s1 and $s2 because $err")
    }


  generateTrafficSpikeReport("server1.rockthejvm.com","server2.rockthejvm.com").value.foreach(println)
  generateTrafficSpikeReport("server1.rockthejvm.com","server3.rockthejvm.com").value.foreach(println)
  generateTrafficSpikeReport("server2.rockthejvm.com","server3.rockthejvm.com").value.foreach(println)
  generateTrafficSpikeReport("server4.rockthejvm.com","server2.rockthejvm.com").value.foreach(println)

}
