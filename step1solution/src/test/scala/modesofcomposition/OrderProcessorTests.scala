package modesofcomposition

class OrderProcessorTests extends munit.FunSuite {

  type F[X] = IO[X]

  val rabbitCode = "Rabbit"
  val customerIdStr = "12345"

  test("decodeMsg") {
    val msg =
      s"""{
        |"customerId": "$customerIdStr",
        |"skuQuantities": [["${rabbitCode}", 2]]
        |}""".stripMargin

    val orderMsg = OrderProcessor.decodeMsg[F](msg.getBytes).unsafeRunSync()

    val expected = OrderMsg(customerIdStr, zio.NonEmptyChunk(OrderSkuQuantity(rabbitCode -> 2)))

    assertEquals(orderMsg, expected)
  }
}
