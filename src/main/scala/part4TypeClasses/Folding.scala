package part4TypeClasses

import cats.{Eval, Monoid}

object Folding extends App {

  // TODO: Implement in terms of foldLeft or foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft(List.empty[B])((a, b) => a ++ List(f(b)))

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      //list.foldLeft(List.empty[B])((a, b) => a ++ f(b))
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldLeft(List.empty[A])((a, b) =>
      if (predicate(b)) a ++ List(b) else a)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((a, b) => monoid.combine(a, b))
  }

  val starterList = List(1, 2, 3, 4, 5)
  import cats.Monoid
  //import cats.instances.int._
  implicit val multiplyem = Monoid.instance[Int](1, (x, y) => x * y)
  println(ListExercises.map(starterList)(_ + 2))
  println(ListExercises.flatMap(starterList)(item => List(item, item * 100, item * 3000)))
  println(ListExercises.filter(starterList)(_ > 3))
  println(ListExercises.combineAll(starterList))


  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]

  Foldable[List].foldLeft(List(1,2,3),0)(_ + _) // 6
  import cats.instances.option._ // implicit Foldable[Option]
  val foldedOpt = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32
  println(foldedOpt)

  // fold right is stack safe regardless of container implementation (because of eval)
  val rightFold : Eval[Int] = Foldable[List].foldRight(List(1,2,3), Eval.now(0)){
    (num, eval) => eval.map(_ + num)
  }

  val antotherSum = Foldable[List].combineAll(List(1,2,3)) // implicit Monoid[Int]
  println(antotherSum)

  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))( _.toString) // implicit Monoid[String]
  println(mappedConcat)

  import cats.instances.vector._
  val intsNested = List(Vector(1,2,3), Vector(4,5,6))
  // compose allows different foldables (in this case List and Vector) to be acted on jointly
  val allOfEm = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)
  println(allOfEm)

  // extension methods
  import cats.syntax.foldable._

  val sum3 = List(2,3,4).combineAll // req Foldable[List], Monoid[Int]
  println(sum3)

  val mappedConcat2 = List(9,9,9).foldMap(_.toString)
  println(mappedConcat2)
}