package modesofcomposition

import zio.{RIO, ZIO}

/** Certifies the id string refers to a Customer  */
case class Customer private[modesofcomposition](id: String, region: CustomerRegion)

/** Validates a Customer string is a valid customer */
object CustomerLookup {
  trait Service {

    def resolveCustomerId(customerId: String): zio.IO[AppError, Customer]
  }

  def resolveCustomerId(customerId: String): ZIO[CustomerLookup, AppError, Customer] = 
    ZIO.accessM(_.get.resolveCustomerId(customerId))

}
