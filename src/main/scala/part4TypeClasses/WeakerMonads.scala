package part4TypeClasses

import cats.{Applicative, Apply}

object WeakerMonads extends App {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](value: M[A])(f: A => M[B]): M[B]

    // TO DO
    def ap[A, B](wf: M[A=>B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f=> f(a)))
    //         |  |        /   \     \/
    //         |  |    M[A=>B] A=>B  B
    //         |  |    \_____   ____/
    //       M[A] A =>      M[B]
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]{
    //def pure[A](value: A): M[A] NOT FUNDAMENTAL TO MONAD
    //def flatMap[A, B](value: M[A])(f: A => M[B]): M[B] //ALSO NOT FUNDAMENTAL: assigned in FlatMap class
    override def map[A, B](value: M[A])(f: A => B): M[B] = flatMap[A, B](value)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._  // provides flatMap extension  method
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_]: FlatMap](numbers: M[Int], chars: M[Char]) : M[(Int, Char)] =
    for {
      n  <- numbers
      c <- chars
    } yield (n, c)

  def getPairsTA[M[_]: FlatMap, A, B](ma : M[A], mb: M[B]) : M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)


}
