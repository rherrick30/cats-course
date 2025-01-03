package part4TypeClasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]) = {
      val functionWrapper: W[B => (A,B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }
    // fundamental
    def ap[B, T](wf: W[B=>T])(wa: W[B]): W[T]

    //TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper){  t => f(t._1, t._2)}
    }
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A]// fundamental
  }

  import cats.Apply
  import cats.instances.option._

  val applyOption = Apply[Option]
  val functApp = applyOption.ap(Some((x: Int) => x +1))(Some(2))
  println(functApp)

  import  cats.syntax.apply._
  val tupleOfOptions = (Option(1), Option(2), Option(3))

  val optionOfTuple = tupleOfOptions.tupled // unwrapps options and rewraps
  println(optionOfTuple)
  println(tupleOfOptions.mapN(_ + _ + _))



}
