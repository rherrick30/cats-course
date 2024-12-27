package part3DataManipulation

import cats.data.Reader

object Readers extends App {

  /* Imagine we have ..
   - configuration file => Initial data structure
   - DB layer
   - Http layer
   - Business logic layer

   so the config file output feeds the other layers:
   Readers support this paradigm
   */

  case class Configuration(dbUserName: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long) : String = "dispatched" // select * from db table and return the status of the order
    def getLastOrderId( username: String): Long = 99999
  }
  case class HttpService(host: String, port: Int){
    def start(): Unit = println("server started")
  }

  val config = Configuration("user", "pass", "localhost", 8081, 8, "robherrick@hotmail.com")
  // insert a Cats reader for the configuration
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader( cfg => DbConnection(cfg.dbUserName,cfg.dbPassword))
  // then if you wanted to get a connection, inject a configuration object into the reader
  val dbConn : DbConnection = dbReader(config)

  // Reader[I, O]
  // can also use map to transform the response by calling a instance method
  val robsOrderStatusReader : Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val robsOrderStatus: String  = robsOrderStatusReader.run(config)

  // RJH: Hmmm....can you use parameters???
  val generalStatusReader : Long => Reader[Configuration, String] = id => dbReader.map(dbConn => dbConn.getOrderStatus(id))
  // RJH: yup

  // Can chain them too
  def getLastOrderStatus(uid: String) :  String  = {
    val usersLastOrderId: Reader[Configuration, String] =
      dbReader.map(_.getLastOrderId(uid))
      .flatMap(orderId => dbReader.map(_.getOrderStatus(orderId)))

    val usersLastOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(uid))
      orderStatus <- dbReader.map((_.getOrderStatus(lastOrderId)))
    } yield orderStatus

    usersLastOrderId.run(config)
    usersLastOrderFor.run(config)
  }

  println(getLastOrderStatus("herrick"))


  /*
  Pattern:
  1. create initial data structure
  2. create a reader which specifies how that data structure will be manipulated later
  3. then map and flatMap the reader to produce derived information
  4.  when the info is needed, then run the reader with the initial data structure
   */

  // TODO 1: email a user
  case class EmailService(emailReplyTo: String){
    def sendEmail(address: String, contents: String) = s"From $emailReplyTo to $address >>> $contents"
  }

  val emailReader: Reader[Configuration, EmailService] = Reader( cfg => EmailService(cfg.emailReplyTo))
  def emailUser(username: String, userEmail: String): String ={
    // fetch status of last order
    // email them with the email status
    val sendEmailStatusReader : Reader[Configuration, String] =
      for {
        lastOrderId <- dbReader.map(_.getLastOrderId(username))
        orderStatus <- dbReader.map((_.getOrderStatus(lastOrderId)))
        sentEmail <- emailReader.map(_.sendEmail(userEmail,s"your order status is '${orderStatus}''" ))
      } yield sentEmail

    sendEmailStatusReader.run(config)
  }
  println(emailUser("rherrick", "rjhrjh@rjh.com" ))

  // TODO 2: What programming pattern do readers remind you of?
  // dependency injection

}

