package modesofcomposition

import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

import TestSupport._

object OrderProcessTestZ extends DefaultRunnableSpec {

  def spec = suite("Step2")(

    testM("resolveOrderMsg") {
      val orderMsg = OrderMsg(usCustomerIdStr, NonEmptyChunk(
        OrderSkuQuantity(koalaCode, 1),
        OrderSkuQuantity(hippoCode, 2),
      ))
      val expected = new CustomerOrder(usCustomer,
        NonEmptyChunk(
          SkuQuantity(toyKoala, PosInt(1)),
          SkuQuantity(toyHippo, PosInt(2)),
        ))

      for {
        order <- OrderProcessor.resolveOrderMsg(orderMsg)
      } yield assert(order)(equalTo(expected))
    }.provideCustomLayer(defaultLayer) 

  )
}
