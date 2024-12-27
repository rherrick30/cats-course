package part3DataManipulation

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.Accumulator

object Writers extends App {

  import cats.data.Writer
  import cats.instances.vector._

  // define at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started Something"), 45)

  //manipulate with pure FP
  val anIncreaseWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // values stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "adding with both", _ * 2.5) // both value and logs change
  val aWriterWIthBoth2 = aWriter.mapBoth { (logs, value) => {
    (logs :+ "found something interesting", value + 45)
  }
  }
  println(aWriterWithBoth)
  println(aWriterWIthBoth2)

  // 3 - dump either value or logs
  println(s"The value is ${aWriter.value}")
  println(s"The logs are ${aWriter.written}")
  val (l, v) = aWriter.run
  println(s"l is ${l}\nwhile v is ${v}")

  // writers support map and flatMap so you can do crap like this:
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1", "Log B2"), 40)
  val compositeWriter = for {
    a <- writerA
    b <- writerB
  } yield a + b
  println(s"Composite:$compositeWriter.run")

  // reset will clear the logs (not the value)
  // only works in the context of a Monoid so lets import one for List

  import cats.instances.list._

  val anEmptyWriter = aWriter.reset
  println(s"anEmptyWriter is $anEmptyWriter")

  // TODO 1: Rewrite something which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }



  def countAndLogMyFirst(n: Int): Writer[Vector[String], Int] = {
    var wrtInit = Writer(Vector[String](), n)
    @tailrec
    def updateWrtr(wrt:  Writer[Vector[String], Int]) :  Writer[Vector[String], Int] =
      if( wrt.value <= 0) wrt
      else {
        updateWrtr(wrt.bimap( Vector(wrt.value.toString) ++ _ , _ - 1))
      }
    updateWrtr(wrtInit)
  }


  def countAndLog(n: Int) : Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  @tailrec
  def countAndLogTailRec(n: Int, accumulator: List[Writer[Vector[String], Int]]) : Writer[Vector[String], Int] = {
    if(n<=0){
        accumulator.reduce( (a,b) => {
          //Writer( a.written ++ b.written, n)
          for{
            w1 <- a
            w2 <- b
          } yield (w1 + w2)
        })
    }
    else{
      countAndLogTailRec(n - 1, List(Writer(Vector(s"$n"), n)) ++ accumulator)
    }
  }

  println("*** TO DO 1 base ***")
  countAndSay(8)
  println("*** TO DO 1 mine ***")
  //val resultWriter = countAndLogMine(8)
  val resultWriter = countAndLogTailRec(8, List.empty)
  resultWriter.written.foreach(println)

  // Benefit #1: we work with pure FP

  // TODO 2: convert naiveSum with writers
  def naiveSum(n: Int) : Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum (${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  @tailrec
  def naiveWithWritersMine(n: Int, accumulator: List[Writer[Vector[String], Int]] ) : Writer[Vector[String], Int] = {
//Int    val naiveWriter : Writer[Vector[String], Int] = Writer(Vector.empty, 0)
//    List(1 to n).foreach(i => {
//    })
    //naiveWriter

    if(n<=0){
//      val retval = accumulator.reduce( (a,b) => {
//        Writer( a.written ++ b.written ++ Vector(s"Computed sum (${b.value - 1}) = ${a.value + b.value}"), a.value + b.value)
//      })
//      retval

      accumulator.fold(Writer[Vector[String], Int](Vector.empty, 0))(

        (b, a) => {
          // println(s".......a is ${a}; b is ${b}")
          Writer( a.written ++ b.written ++ Vector(s"Computed sum (${a.value}) = ${a.value + b.value}") , a.value + b.value)
        }
      )


    }
    else{
      naiveWithWritersMine(n - 1, List(Writer(Vector(s"Now at $n"), n)) ++ accumulator)
    }
  }

  def sumWithLogs(n:Int) : Writer[Vector[String], Int] = {
    if(n<=0){
      Writer(Vector(), 0)
    }else{
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computed sum (${n - 1}) = $lowerSum"), n)
      } yield n + lowerSum   // n + lowerSum yields a Writer.  The other writers although they are not used, add to the writers
      // becaused they composed via semigroup in the implicit map/flatMap of the "for"
    }
  }


  println("**** Naive sum base")
  naiveSum(5)

  println("*** log with writers")
  sumWithLogs(5).written.foreach(println)

  println("**************** FUTURES *********************")
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val SIZE = 0

  val fut1 = Future(naiveSum(SIZE)).foreach(println)
  val fut2 = Future(naiveSum(SIZE)).foreach(println)

  println("**** NOW WITH THE WRITER")
  Future(sumWithLogs(SIZE)).map(_.written).foreach(println)
  Future(sumWithLogs(SIZE)).map(_.written).foreach(println)

  println("*** naive w writers")
  naiveWithWritersMine(5, List.empty).written.foreach(println)

}
