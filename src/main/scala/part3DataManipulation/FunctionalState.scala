package part3DataManipulation

object FunctionalState extends App {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay : State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  (1 to 10).toSeq.map(countAndSay.run(_).value).toSeq.foreach(v => println(s"${v._2} -> ${v._1}"))

  // Bad with vars
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10 obtained $a"
  a *=5
  val secondComputation = s"..and multiplied by 5 to get $a"

  // Pure FS with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to ${s} obtained ${s+1}"))
  val secondTransformation = State((c : Int) => (c * 5, s"mutiplied $c by 5 to get ${c*5}"))

  val compositeTransformation = firstTransformation.flatMap {
    firstResult=> secondTransformation.map(secondResult=> (firstResult,secondResult))
  }

  println(compositeTransformation.run(10).value)
  val sameTransWithFor = for {
    erst <- firstTransformation
    nd <- secondTransformation
  } yield (erst, nd)
  println(sameTransWithFor.run(18).value)

  val func1 = (s: Int) => (s + 1, s"Added 1 to ${s} obtained ${s+1}")
  val func2 = (c : Int) => (c * 5, s"mutiplied $c by 5 to get ${c*5}")
  val compositeFunc = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }
  println(compositeFunc(10)) // produces deeply nested tuple!

  // TODO 1: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addtoCart(item: String, price: Double) : State[ShoppingCart,Double] =
    State((cart :ShoppingCart) => (ShoppingCart( cart.items :+ item, cart.total + price), cart.total + price ))

  val mtCart = ShoppingCart(List.empty, 0)
  val shopper = for {
    _ <- addtoCart("Fender", 1300)
    _ <- addtoCart("Gibson", 2000)
    itm3 <- addtoCart("Genuine PRS", 3100)
  } yield itm3  // had i done item1 + item2 etc. it would be wrong because it would sum total twice
  println(shopper.run(mtCart).value)

  // TODO 2: pure mental gymnastics
  // inspect() does NOT change the state, but outputs f(a) as B
  // get() returns the value of that state and makes no changes
  // set() updates the state and returns Unit (as B)
  // modify() updates the state to f(a)
    def inspect[A,B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))
    def get[A] : State[A, A] = State((a: A)=> (a, a))
    def set[A](value: A) : State[A, Unit] = State((_: A)=> (value, ()))
    def modify[A](f: A => A) : State[A, Unit] = State((a: A)=>(f(a),()))

  // all the above methods are available in ...
  import cats.data.State._

  val programme: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  // programme is the functional representation of a normal sequential program...but using IMMUTABLE values

}
