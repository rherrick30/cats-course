package part1intro

object TCVariance extends App {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  //val invalidComparison = Some(2) === None // Eq[Some[Int]] not found

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: Subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, thus Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to generic type
  class Vet[-T]
  val doctor : Vet[Cat] = new Vet[Animal]// Cat <: Animal, thus Vet[Animal] <: Vet[Cat]
  // in other words, any operation on any Animal will work on a cat

  // rule of thumb: if a TC-> Has a T?  = covariant, ACTS on T? contravariant
  // vcariance affect how TC instances are being fetched

  /// contravariant TC
  trait SoundMaker[-T]  // contravariant because it appears to work ON T
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]) : Unit = println("wow!")
  makeSound[Animal] // TC instance defined above
  makeSound[Cat] // TC instance for Animal also applies here. (AnimalSoundMaker[Animal] <: AnimalSoundMaker[Cat])
  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]] // since Some <: Option and SoundMaker is contravariant, this works

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere!"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]) : String = event.show
  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present

  // rule 3:  You cannot have both benefits (of co and contra variance)
  // Cats uses Invariant TC's
  // if you want to compare Options to empty Options use general type with smart constructors
  println(s"should be false: ${Option(2) === Option.empty[Int]}")


  //def main(array: Array[String]) : Unit = {
    println(organizeShow[Cat]) // ok: the compiler will inject CatsShow as implicit
    //println(organizeShow[Animal]) // will not compile.  There are 2 potential implementations (GeneralAnimalShow and CatsShow)
  //}

}
