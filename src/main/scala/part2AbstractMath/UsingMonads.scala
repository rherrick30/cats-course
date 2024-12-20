package part2AbstractMath

object UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(1)
  val aExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  val aManualEither : Either[String, Int] = Right(42)
  // because these Type Instances look like "Options" or "Trys"
  // (if you block out the Right Value), they are available as Monads
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEitherMonad = loadingMonad.pure(45)
  val aChangedLoading = loadingMonad.flatMap(anEitherMonad)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life"))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long)  :  LoadingOr[OrderStatus] =
    Right(OrderStatus ( orderId, "ReadyToShip"))
  def trackLocation(orderStatus: OrderStatus) : LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available as yet") else  Right("on zee vay!")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)
  // ordinarily you'd want to import extension methods for flatMap and Map.
  // but Either already has those as its part of the Standard Library
  val orderLocationAlternative = for{
    status <- getOrderStatus(orderId)
    location <- trackLocation(status)
  } yield location
  println(s"The order status is ${orderLocation} whilst the alternate status is ${orderLocationAlternative}")

  // TODOL the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection( cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // NOW we need the extension methods to use "for" in our generalized response methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]) : M[String] =
    for {
    conn <- service.getConnection(config)
    resp <- service.issueRequest(conn, payload)
  } yield resp

  // DO NOT CHANGE THE CODE ABOVE

  /*
    - If the host and port are found in teh configuration map, return an M with them
      otherwise it will fail, according to the logic of M (option->none, try,future->failure, either->left)
    - The issue request returns a M containing the string "request (payload) has been accepted if payload < 20 chars
      otherwise its a fail as above

      TODO: Provide a real implementation of HttpService using Try, Option, Future or Either
  * */
  import cats.instances.option._
  val optionMonad = Monad[Option]

  object httpServiceOption extends HttpService[Option] {
    override def getConnection( cfg: Map[String, String]): Option[Connection] =
      /*(if(cfg.contains("host") && cfg.contains("port"))
        Some(Connection(cfg("host"), cfg("port")))
      else None
    */
    for {
      h <- cfg.get("host")
      p <- cfg.get("port")
    } yield Connection(h,p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if(payload.length<20)
        Some(s"request ($payload) has been accepted")
      else
        None
  }

  private def getResponseOld( payload: String) : Option[String] = httpServiceOption.getConnection(config).flatMap(httpServiceOption.issueRequest(_,payload))
  private def getResponseFor(payload: String) : Option[String] = for {
    cfg <- httpServiceOption.getConnection(config)
    resp <- httpServiceOption.issueRequest(cfg,payload)
  } yield resp


  println(s"Good response: ${getResponseFor("Its short enough")}")
  println(s"Bad response: ${getResponseFor("This is entirely too long to get a good response")}")

  // TODO: Implement another HttpService with LoadingOr or ErrorOr
  object httpServiceEither extends HttpService[LoadingOr] {
    override def getConnection( cfg: Map[String, String]): LoadingOr[Connection] =
      (if(cfg.contains("host") && cfg.contains("port"))
        Right(Connection(cfg("host"), cfg("port")))
      else Left("Cannot create a connection"))

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      if(payload.length<20)
        Right(s"request ($payload) has been accepted")
      else
        Left("could not process the payload")
  }

  def issueRequestEither(payload: String) = httpServiceEither.getConnection(config) match {
    case Right(c) => httpServiceEither.issueRequest(c, payload)
    case Left(msg) => Left(msg)
  }

  def issueRequestEitherFor(payload: String) : LoadingOr[String] = for {
    conn <- httpServiceEither.getConnection(config)
    resp <- httpServiceEither.issueRequest(conn, payload)
  } yield resp

  println(s"Good response (with either): ${issueRequestEitherFor("Its short enough")}")
  println(s"Bad response (with either): ${issueRequestEitherFor("This is entirely too long to get a good response")}")

  // NOW WITH THE GENERIC
  println(s"Good response (with generic): ${getResponse(httpServiceEither, "Its short enough")}")
  println(s"Bad response (with generic): ${getResponse(httpServiceOption, "This is entirely too long to get a good response")}")

}
