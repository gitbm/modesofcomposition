package modesofcomposition

import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

import java.time.Instant
import TestSupport._

object OrderProcessTestZ extends DefaultRunnableSpec {

  def spec = suite("Step3")(

    testM("processCustomerOrder - unavailable") {
      val order = CustomerOrder(usCustomer, NonEmptyChunk(SkuQuantity(toyKoala, refineMV[Positive](1))))
      val expected = Chain(Unavailable(NonEmptySet.of(toyKoala), order, Instant.ofEpochMilli(currMillis))).asRight[io.circe.Error]

      for {
        _ <- OrderProcessor.processCustomerOrder(order)
        testState <- TestState.get
        msgs = testState.getMessages(Topic.Unavailable).traverse(fromJsonBytes[Unavailable])
      } yield assert(msgs)(equalTo(expected))
    }.provideCustomLayer(defaultLayer) 

  )
}




  // test("processCustomerOrder - unavailable") {
  //   val order = CustomerOrder(usCustomer, NonEmptyChunk(SkuQuantity(toyKoala, refineMV[Positive](1))))

  //   val testState = run(OrderProcessor.processCustomerOrder(order) *> TestState.get)
    
  //   val expected = Chain(Unavailable(NonEmptySet.of(toyKoala), order, Instant.ofEpochMilli(currMillis))).asRight[io.circe.Error]

  //   assertEquals(testState.getMessages(Topic.Unavailable).traverse(fromJsonBytes[Unavailable]), expected)
  // }
