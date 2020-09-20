package modesofcomposition

import java.util.UUID
import java.time.Instant

import cats.effect.concurrent.Ref

import zio.NonEmptyChunk

import TestSupport._

class OrderProcessorTests extends munit.FunSuite with TestSupport {

  test("processCustomerOrder - unavailable") {
    val order = CustomerOrder(usCustomer, NonEmptyChunk(SkuQuantity(toyKoala, refineMV[Positive](1))))

    val testState = run(OrderProcessor.processCustomerOrder(order) *> TestState.get)
    
    val expected = Chain(Unavailable(NonEmptySet.of(toyKoala), order, Instant.ofEpochMilli(currMillis))).asRight[io.circe.Error]

    assertEquals(testState.getMessages(Topic.Unavailable).traverse(fromJsonBytes[Unavailable]), expected)
  }

}
