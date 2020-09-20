package modesofcomposition

import zio.Runtime.default.unsafeRun

import TestSupport._

class OrderProcessorTests extends munit.FunSuite {

  val rabbitCode = "Rabbit"
  val customerIdStr = "12345"

  test("decodeMsg") {
    val msg =
      s"""{
        |"customerId": "$customerIdStr",
        |"skuQuantities": [ { "name": "${rabbitCode}", "quantity": 2 } ]
        |}""".stripMargin

    val orderMsg = run(OrderProcessor.decodeMsg(msg.getBytes))
    val expected = OrderMsg(customerIdStr, zio.NonEmptyChunk(OrderSkuQuantity(rabbitCode, 2)))

    assertEquals(orderMsg, expected)
  }
}
