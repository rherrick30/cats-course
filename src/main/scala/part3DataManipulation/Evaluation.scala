package part3DataManipulation

import cats.{Always, Later, Now}

object Evaluation {

  /*
  Cats makes the distinction between:
  - evaluating an expression eagerly
  - evaluating lazily and every time you request it
  - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval

  // eager evaluation
  val instantEval: Eval[Int] = Eval.now {
    println("computing now!")
    64535
  }


  // always (lazy) valuation
  val redoEval = Eval.always{
    println("computing again")
    424
  }

  // memoized valuation
  val memoEval = Eval.later{
    println("computing later")
    99999
  }
  val composedEval = instantEval.flatMap(value1 => memoEval.map(_ +  value1))

  val anotherComposedValuation = for {
    value1 <- instantEval
    value2 <- memoEval
  } yield value1 + value2

  // TODO 1
  val evalEx1 = for {
    a <- memoEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d
  // computing now computing later computing again computing again

  // This holds the value without recalculation
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always { println("Step 1..."); "put the guitar on your lap" }
    .map {step1 => println("Step 2"); s"$step1 then put your left hand on the neck" }
    .memoize // remember the value up to this point
    .map { steps12 => println("Step 3, more complicated"); s"$steps12 and then with the right hand strike the strings"}

//  println(tutorial.value)
//  println(tutorial.value)

  // TODO 2: Implement defer such that defer(Eval.now) does NOT run the side effects.
  def defer[T] (eval: => Eval[T]) : Eval[T] = Eval.later().flatMap(_ => eval)

  val test2 = defer(Eval.now {
    println("Now!")
    42
  })
  //println(test2.value)

  // TODO 3: rewrite the following method with evals
  def reverseList[T](list: List[T]) : List[T] = {
    if(list.isEmpty){
      list
    } else {
        reverseList(list.tail) :+ list.head
    }
  }

  def reverseEvalMine[T](list: List[T]) : Eval[List[T]] = {
    if(list.isEmpty){
      Eval.later(list)
    } else {
      Eval.later(reverseEvalMine(list.tail).value :+ Eval.later(list.head).value)
    }
  }

  def reverseEval[T](list: List[T]) : Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else reverseEval(list.tail).map(_ :+ list.head)
  }

  // Cats can make the above stack safe like so (with defer)
  // because it is creating a chain of eval.later.flatmap. which are evaluated
  // in a tail recursive manner.
  def reverseEvalDef[T](list: List[T]) : Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEvalDef(list.tail).map(_ :+ list.head))
  }

  def sampleLst = List(1,2,3,4,5)
  println(reverseList(sampleLst))
  println(reverseEval(sampleLst).value)

  println(reverseEvalDef((1 to 50000).toList).value)


  def main(argv: Array[String]) : Unit = {
//    println(instantEval.value)
//    println(redoEval.value)
//    println(redoEval.value)
//    println(memoEval.value)
//    println(memoEval.value)
//    println("============================")
//    println(composedEval.value)
//    println(composedEval.value)
//    println(anotherComposedValuation.value)
//    println(evalEx1.value)
//    println(evalEx1.value)

  }


}
