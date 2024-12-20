package part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads extends App {

  // list
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1 :  How do you create all combinations of (number, char)?
  val allCombosChain = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val allCombosInitial = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  println(s"all combos  : ${allCombosInitial}")
  println(s"same list as: ${allCombosChain}")
  // those 2 methods are IDENTICAL because the compiler
  // collapses a for comprehension to chained flatmaps
  // with a map on the end

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: How do you create the combination of (number, char)
  val comboOption = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)
  val comboOptionChain = numberOption.flatMap(n => charOption.map(c => (n, c)))
  println(s"comboOption      ${comboOption}")
  println(s"comboOptionChain ${comboOptionChain}")

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def futA: Future[Int] = Future(3)

  def futB: Future[Char] = Future('b')

  val futureFor = for {
    n <- futA
    c <- futB
  } yield (n, c)
  val futureChain = futA.flatMap(n => futB.map(c => (n, c)))
  futureFor.map(f => println(s"futfor${f}"))
  futureChain.map(f => println(s"futchn${f}"))

  /*
  The pattern with the above has 2 fundamental operation
  1. Ability to wrap a value into a M value
  2. Transform M value to a different M value with flatmap

  The Cats typeclass that embodies these 2 operations is called a...
   MONAD
  */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](value: M[A])(f: A => M[B]): M[B]

    def map[A, B](value: M[A])(f: A => B): M[B] = flatMap[A, B](value)(a => pure(f(a)))
  }

  // cats monad

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._

  val listMonad = Monad[List]
  val aList = listMonad.pure(5)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))
  println(s"\nmonadic list fun:  ${aList}, trans: ${aTransformedList}")

  // TODO: use a Monad[Future]

  import cats.instances.future._

  val futureMonad = Monad[Future] // note requires implicit execution context
  val aFuture = futureMonad.pure(4)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  aFuture.map(f => println(s"pure->${f}"))
  aTransformedFuture.map(f => println(s"transformed->${f}"))

  // Specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map((n, _)))
  // can do an option or future too...but you need separate values

  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  println("using pairs fx:")
  println(getPairs(numbersList, charsList))
  println(getPairs(numberOption, charOption))
  getPairs(futA, futB).foreach(println)

  // extension methods - wierder imports - pur, flatmap

  import cats.syntax.applicative._ // pure is here

  val oneOption = 1.pure[Option] // implicit monad option in scope

  import cats.syntax.flatMap._ // flat map is here obviously

  val optionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO: Implement the map method in MyMonad
  // (see above)

  import cats.syntax.functor._ // map is here. (Monads are Functors)

  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // because Monads have access to map + flatmap, we can use for comprehensions...
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two
  println(s"The old ${composedOptionFor}")

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsShorter[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma   // because the implicit is in scope by limiting M[_]
    b <- mb
  } yield (a, b)

  println("using pairsShorter fx:")
  println(getPairsShorter(numbersList, charsList))
  println(getPairsShorter(numberOption, charOption))
  getPairsShorter(futA, futB).foreach(println)

}
