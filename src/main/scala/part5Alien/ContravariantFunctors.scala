package part5Alien

import cats.Monoid

object ContravariantFunctors extends App {

  trait Format[T] { self =>
    def format(value : T) : String
    def contramap[A](func: A => T) : Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = value match {
      case true => "truth"
      case false => "fallacy"
    }
  }
  println(format("So far so good"))
  println(format(true))
  println(format(99))

  import cats.instances.option._
  import cats.instances.int._
  // problem: given we have Format[MyType] can we have Format[Option[MyType]]
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]) : Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))



  println(format(Option(43)))
  // note the below nesting is applied in stack (reverse) order.
  // so Option(Option(Int) => Option(Int) => Int
  // thats why its called "contramap" instead of "map"
  println(format(Option(Option(33))))

  import cats.Contravariant
  import cats.Show

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

}

