val m : Map[String,String] = Map(
  "name" -> "rob",
  "game" -> "today its guitar",
  "job" -> "progrmmah"
)


println(s"My name is ${m("name")}")
println(s"do I have a job ${if (m.contains("life") && m("life").nonEmpty)"yes" else "no"}")



import cats.syntax.all._

val f: (Int, Char) => Double = (i, c) => (i + c).toDouble

val int: Option[Int] = Some(5)
val char: Option[Char] = Some('a')
int.map(i => (c: Char) => f(i, c)) // what now?

import cats.Apply
import cats.syntax.apply._
import cats.instances.vector._
val listApply = Apply[Vector]

( Vector(1,2,3), Vector(4,3,4)).mapN(_ + _)


