package part3DataManipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation extends App {

  import cats.data.Validated
  // like an Either: Left is error and right is .... right!
  val aValidValue: Validated[String, Int] = Validated.valid(42) // right value
  val aCrappyValue : Validated[String, Int] = Validated.invalid("not an int, genius!") //left value
  val aTest: Validated[String, Int] = Validated.cond(42> 39, 99, "meaning of life too small")

  def isPrime(num: Int) : Boolean = {
    @tailrec
    def isPrimeHelper(potDivisor: Int) : Boolean = {
      if(potDivisor>num / 2) true
      else if (num % potDivisor == 0) false
      else isPrimeHelper(potDivisor+1)
    }
    if(num <= 0 || num == 1 ) false
    else isPrimeHelper(2)
  }

  // TODO 1: Use Either:  Must be prime, non negative, n<=100, even
  def testNumber(number: Int) : Either[List[String], Int] = {
    val testResults = List[Int => Either[List[String], Int]](
      n => if(n >= 0) Right(n) else Left(List(s"$n is negative")),
      n => if(n<= 100) Right(n) else Left(List(s"$n is greater than 100")),
      n => if(n % 2 == 0) Right(n) else Left(List(s"n is odd")),
      n => if( isPrime(n)) Right(n) else Left(List(s"n is not prime"))
    )
    testResults.map(f=> f(number)).reduce( (a, b) => {
      b match {
        case Right(_) => a match {
          case Right(aval) => Right(aval)
          case Left(aval) => Left(aval)
        }
        case Left(bval) => a match {
          case Right(_) => Left(bval)
          case Left(aval) => Left( aval ++ bval)
        }
      }
    })
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int) : Validated[List[String], Int] = List(
    Validated.cond(n >= 0, n, List(s"$n is negative")),
    Validated.cond(n<= 100,n,  List("$n is greater than 100")),
    Validated.cond(n % 2 == 0,n, List("n is odd")),
    Validated.cond(isPrime(n),n,List("n is not prime"))
  ).reduce( (a,b) => a.combine(b))

  List(1,2,3,101,-99).foreach(n=> println(s"$n -> ${testNumber(n)}"))
  List(1,2,3,101,-99).foreach(n=> println(s"$n -> ${validateNumber(n)}"))

  // validations can be chained
  aValidValue.andThen(_ => aCrappyValue)
  // test
  aValidValue.ensure(List("something is wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(v => v + 1)
  aCrappyValue.leftMap(l => l.length)
  aValidValue.bimap(_.length, _ +  1)
  // interoperate w/ standard lib:
  val eitherToBeEvaluated : Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToBeEvaluated : Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present"))
  val tryToBeEvaluated :  Validated[Throwable, Int] = Validated.fromTry( Try {"something".toInt})
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 form validation
  implicit val combineString: Semigroup[String] = Semigroup.instance[String]((a, b) => if(a.contains(b)) a else s"$a$b")
  object FormValidationMine {
    type FormValidation[T] = Validated[List[String], T]
    /*
    fields are: name email and password
    rules:
      - name email and password must me specified
      - name mustnt be blank
      - email must have @
      - password must have >=10 characters
     */
    def validateForm(form: Map[String, String]) : FormValidation[String] = List(
      Validated.cond(form.contains("Name"),"Success", List("Must Specify a name")),
      Validated.cond(form.contains("Email"),"Success", List("Must Specify a email")),
      Validated.cond(form.contains("Password"),"Success", List("Must Specify a password")),
      Validated.cond(form.contains("Name") && form("Name").nonEmpty,"Success", List("Name must be > 0")),
      Validated.cond(form.contains("Email") && form("Email").contains('@'),"Success", List("email must have @")),
      Validated.cond(form.contains("Password") && form("Password").length>=10,"Success", List("password must be >= 10")),
    ).reduce( (a,b)=> b.combine(a))
  }

  object FormValidationCourse {
    type FormValidation[T] = Validated[List[String], T]
    import cats.instances.string._
    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank."))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Passwords must be at least 10 characters long."))

    /*
      fields are
      - name
      - email
      - password

      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name").andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailProperForm))
        .combine(getValue(form, "Password").andThen(passwordCheck))
        .map(_ => "Success")
  }

  val form  = Map(
    "Name" -> "Rob",
    "Email" -> "r@h.com",
    "Password" -> "thisisareallyweakpwd"
  )

  println(FormValidationMine.validateForm(form))
  println(FormValidationCourse.validateForm(form))

  // extension methods
  import cats.syntax.validated._
  val aValidMeaningOfLife : Validated[List[String],Int] = 42.valid
  val aInvalidMeaningOfLife : Validated[List[String],Int] = List("Unknown").invalid
  val error : Validated[String, Int] = "Something is amiss".invalid[Int]
}
