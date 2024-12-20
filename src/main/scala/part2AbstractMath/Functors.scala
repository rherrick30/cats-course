package part2AbstractMath

import scala.util.Try

object Functors extends App{

  val sampleList = List(1,2,3)
  val sampleOpt = Option(2)
  val sampleTry = Try(42)

  val aModifiedList = sampleList.map(_ + 1)
  val aModifiedOption = sampleOpt.map(_ + 1)
  val aModifiedTry = sampleTry.map(_ + 1)
  println(s"Banality is $aModifiedList, $aModifiedOption and $aModifiedTry")

  // simplified definition of map
  trait myFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B) : B
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(sampleList)(_ + 1)

  import cats.instances.option._
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(sampleOpt)(_ + 1)

  import cats.instances.try_._
  def incrementedTry = Functor[Try].map(sampleTry)(_ + 1)

  println(s"With functor: $incrementedNumbers; $incrementedOption; $incrementedTry")

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(list: Option[Int]): Option[Int] = list.map(_ * 10)
  def do10xTry(list: Try[Int]): Try[Int] = list.map(_ * 10)

  // Functors allow you to write a simple def instead of one for each type
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  println(do10x(sampleList))
  println(do10x(sampleOpt))
  println(do10x(sampleTry))

  // TODO: define your own functor for a binary tree
  trait Tree[+T]
  // when you have covariant types is a good idea to supply constructors for the other types in a companion object
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]) : Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  // I first did this as a val instead of object
  implicit object treeFunctor extends Functor[Tree]  {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
    }
  }

  val sampleTree: Tree[Int] = Branch( 23, Branch(44, Leaf(1), Leaf(2)), Leaf(3) )
  println(s"Original Tree: $sampleTree")
  println(s"..times ten: ${do10x(sampleTree)}")
  // note '[Tree]' needs to be specified because cats as a rule is invariant!
  println(s"..with a branch: ${do10x[Tree](Branch(7, Leaf(8), Leaf(9)))}")
  // Using the constructor methods from the companion object avoids this tho
  println(s"..with companion object: ${do10x(Tree.branch(8, Tree.leaf(9), Tree.branch(1, Tree.leaf(2), Tree.leaf(3)) ))}")

  // extension method for Functors
  import cats.syntax.functor._
  println(s"incremented tree ${sampleTree.map(_ + 1)}")

  // TODO: Create a shorter version of the do10x method
  // we are now constraining F[_] to Functor type so the compiler injects it.
  // There's no need for implicit anymore, we map directly on the container
  def do10x2d[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  println(s"10x-> ${do10x2d(sampleTree)}")

}
