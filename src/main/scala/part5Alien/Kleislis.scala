package part5Alien

object Kleislis extends App {

  val funct1 : Int => Option[String] = x => if (x % 2 == 0) Some(s"$x si even") else None
  val funct2 : Int => Option[Int] = x => Some(x * 3)

  // func3 is func2 andThen func1
  val plainfunct1 : Int => String = x => if (x % 2 == 0) s"$x si even" else "fail"
  val plainfunct2 : Int => Int = x => x * 3
  val plainfunct3 : Int  => String = plainfunct2 andThen plainfunct1

  // but we cannot say...
  //val funct3 = funct2 andThen funct1
  // ...because of the wrapping (the Option)

  import cats.data.Kleisli
  import cats.instances.option._  // FlatMap[Option]
  val funct1k : Kleisli[Option, Int, String] = Kleisli(funct1)
  val funct2k : Kleisli[Option, Int, Int] = Kleisli(funct2)
  val funct3k : Kleisli[Option, Int, String]= funct2k andThen funct1k

  // convenience Apis
  val multiply = funct2k.map(_ * 2) // x => Option(...).map(_*2):  the map is on the result of the function.
  val chain = funct2k.flatMap(x => funct1k)

  // TODO:  What does the InterestingKliesli below remind you of??
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // hint
  val times2 = Kleisli[Id, Int, Int](x => x * 2)
  val plus4 = Kleisli[Id, Int, Int](x => x + 4)
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4
  println(composedFor(3))

  // ... and the answer is a READER.  A Reader is Kleisli of [Id, A, B]

}
