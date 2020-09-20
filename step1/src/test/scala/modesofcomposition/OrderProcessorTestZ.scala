package modesofcomposition

import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

object OrderProcessTestZ extends DefaultRunnableSpec {

  val rabbitCode = "Rabbit"
  val customerIdStr = "12345"

  def spec = suite("Step1")(
    testM("decodeMsg") {
      val msg =
        s"""{
          |"customerId": "$customerIdStr",
          |"skuQuantities": [ { "name": "${rabbitCode}", "quantity": 2 } ]
          |}""".stripMargin

      val expected = OrderMsg(customerIdStr, zio.NonEmptyChunk(OrderSkuQuantity(rabbitCode, 2)))
      
      for {
        orderMsg <- OrderProcessor.decodeMsg(msg.getBytes)
      } yield assert(orderMsg)(equalTo(expected))
    }
  )
}
