package part5Alien

import cats.Monoid
import cats.syntax.monoid

import java.security.CryptoPrimitive

object InvariantFunctors extends App {

  trait Crypto[A] { self =>
    def encrypt(value: A) : String
    def decrypt(encrypted: String) : A

    def imap[B](back: B => A, forth: A => B) : Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }

  }

  def encrypt[A](value : A)(implicit c: Crypto[A]): String = c.encrypt(value)
  def decrypt[A](value: String)(implicit crypto: Crypto[A]) : A  = crypto.decrypt(value)

  implicit val caesarCypher : Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c=> (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  val encrypted = encrypt("Encrypt me, motherf****r")
  val decrypted = decrypt[String](encrypted)
  println(encrypted)
  println(decrypted)
  /*
  How can we support ints, double, Option[String]?
    */

  implicit val doubleCrypto : Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)
  println(encrypt(Math.PI))
  println( decrypt[Double](encrypt(Math.PI)))

  // TODO: Support Option[String] decryption
  implicit val optionStringCrypto : Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))
  implicit val someStringCrypto : Crypto[Some[String]] = caesarCypher.imap(_.getOrElse(""), Some(_))

  val sampelOption = Some("Fool")
  val encryptedOpt = encrypt(sampelOption)
  println(encryptedOpt)
  println(decrypt[Some[String]](encryptedOpt))

  //TODO: Generalize a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
//  def WRONGgeneralOpt[T](implicit monoid: Monoid[T]) : Crypto[Option[T]] =
//    caesarCypher.imap(_.getOrElse("").toString,
//      strVal => if(strVal.isEmpty) Option(monoid.empty) else Option(strVal.asInstanceOf[T])
//    )

  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]) : Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.instances.double._

  val optionDub : Option[Double] = Option(2.3333)
  println(encrypt(optionDub))
  println(decrypt[Option[Double]](encrypt(optionDub)))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]
  // parms are the instance, then the "forth" (A=>B) and finaly the "back" (B=>A)
  val showOptionString = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  // here are the extension methods
  import cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse())

  // TO DO: Whats the relationship

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A=>B)(back: B=>A) : W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B =>A) : W[B]
    def imap[A, B](wa: W[A])(forth: A=>B)(back: B=>A) : W[B] =
      contramap[A, B](wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { /*covariant functor*/
    def map[A, B](wa: W[A])(forth: A=> B) : W[B]
    def imap[A, B](wa: W[A])(forth: A=>B)(back: B=>A) : W[B] =
      map(wa)(forth)
  }



}
