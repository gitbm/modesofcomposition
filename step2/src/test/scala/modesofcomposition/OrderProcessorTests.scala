package modesofcomposition

import java.util.UUID
import java.time.Instant

import cats.effect.concurrent.Ref
import zio.{Has, NonEmptyChunk}
import zio.Runtime.default.unsafeRun

import TestSupport._

class OrderProcessorTests extends munit.FunSuite with TestSupport {

  test("resolveOrderMsg") {
    val orderMsg = OrderMsg(usCustomerIdStr, NonEmptyChunk(
      OrderSkuQuantity(koalaCode, 1),
      OrderSkuQuantity(hippoCode, 2),
    ))

    val order = run(OrderProcessor.resolveOrderMsg(orderMsg))
    val expected = new CustomerOrder(usCustomer,
      NonEmptyChunk(
        SkuQuantity(toyKoala, PosInt(1)),
        SkuQuantity(toyHippo, PosInt(2)),
      ))

    assertEquals(order, expected)
  }

}
