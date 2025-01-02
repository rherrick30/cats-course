package part4typeclasses

object Applicatives extends App{

  // or "Applicative Functors"
  // Applicatives = Functors + pure method
  // functors have Map method, applicative adds "pure" to this
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)
  println(aList)

  import cats.instances.option._
  val optionApplicative = Applicative[Option]
  val aOtherList = optionApplicative.pure("smoke")
  println(aOtherList)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List]
  println(aSweetList)
  println(2.pure[Option])

  // Monads extends Applicatives.  So Applicatives are rarely used on their own
  // (as Monads are everywhere in cats...my observation).  The exception is Validators
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val sSweetValid =Validated.valid(43)  // like pure
  val sValidMapped = sSweetValid.map(_ + 1) // map

  // so valid is a candidate for Applicative
  val validatedApplicative = Applicative[ErrorsOr]
  val purelyValidated = 34.pure[ErrorsOr]
  println(purelyValidated)


  // TO DO:  thought experiment
  //def ap[W[_], A, B](wf: W[A=>B])(wa: W[A]): W[B] = ???
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    //applicative.map(wa)(a => applicative.map(wb)(b=> (a, b)))
    val functionWrapper: W[B => (A,B)] = applicative.map(wa)(a => (b: B) => (a, b))
    //ap(functionWrapper)(wb)
    applicative.ap(functionWrapper)(wb)
  }


  // Applicative have this ap method-> def ap[W[_], B, T](wf: W[B=>T])(wa: W[B]): W[T]
  // so Applicatives can implement product from Semigroupal and thus extends Semigroupals

  val prod = listApplicative.product(List(1,2,3,4), List("cats", "kitties", "kittens"))
  println(prod)

}
