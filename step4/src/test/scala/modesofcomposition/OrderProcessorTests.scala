package modesofcomposition

import java.time.Instant

import cats.effect.concurrent.Ref

import zio.{ZIO, NonEmptyChunk}
import zio.Runtime.default.unsafeRun

import TestSupport._
class OrderProcessorTests extends munit.FunSuite with TestSupport {

  test("processAvailableOrder - dispatch") {
    val order = CustomerOrder(ausCustomer,
      NonEmptyChunk(SkuQuantity(toyRabbit, refineMV[Positive](1))))
    
    val testState = run(
      Uuids.initSeed(seed) *> 
      OrderProcessor.processAvailableOrder(order) *>
      TestState.get
    )

    val expected = Chain(Dispatched(order, Instant.ofEpochMilli(currMillis), UuidSeed.uuid(seed))).asRight[io.circe.Error]

    assertEquals(testState.getMessages(Topic.Dispatch).traverse(fromJsonBytes[Dispatched]), expected)

    assertEquals(testState.invStock, initialStock.updated(toyRabbit, NatInt(499)) )
  }


  test("processAvailableOrder - backorder") {

    val lowHippoStock = initialStock.updated(toyHippo, NatInt(1))

    val order = CustomerOrder(ausCustomer, NonEmptyChunk(
      SkuQuantity(toyRabbit, PosInt(1)),
      SkuQuantity(toyHippo, PosInt(2)),
    ))

    val testState = run(
      Uuids.initSeed(seed) *> 
      OrderProcessor.processAvailableOrder(order) *>
      TestState.get,
     
      defaultTestState.copy(invStock = lowHippoStock)
    )

    val expected = Chain(Backorder(NonEmptyChunk(
      SkuQuantity(toyHippo, PosInt(1))), order, Instant.ofEpochMilli(currMillis))).asRight[io.circe.Error]

    assertEquals(testState.getMessages(Topic.Backorder).traverse(fromJsonBytes[Backorder]), expected)
    
    assertEquals(testState.invStock, lowHippoStock)
  }

 
  test("backorder") {
    val order = CustomerOrder(ausCustomer, NonEmptyChunk(
      SkuQuantity(toyRabbit, PosInt(1)),
      SkuQuantity(toyHippo, PosInt(2)),
    ))
    val insufficientStocks = NonEmptyChunk(
      InsufficientStock(SkuQuantity(toyHippo, PosInt(2)), NatInt(1)))

    val actual = run(OrderProcessor.backorder(insufficientStocks, order))

    assertEquals(actual, Backorder(NonEmptyChunk(SkuQuantity(toyHippo, PosInt(1))),
      order, Instant.ofEpochMilli(currMillis)))
  }

 
   test("dispatch") {
    val order = CustomerOrder(ausCustomer, NonEmptyChunk(
      SkuQuantity(toyRabbit, PosInt(1)),
      SkuQuantity(toyHippo, PosInt(2)),
    ))
    val actual = run((Uuids.initSeed(seed) *> OrderProcessor.dispatch(order)))
    assertEquals(actual, Dispatched(order, Instant.ofEpochMilli(currMillis), UuidSeed.uuid(seed)))
  }

  test("insufficientsAndTaken") {
    val insufficientStock = InsufficientStock(SkuQuantity(toyHippo, PosInt(2)), NatInt(1))
    val take = SkuQuantity(toyKoala, PosInt(1))
    val taken = List(take)

    val actual = OrderProcessor.insufficientsAndTaken(
      NonEmptyChunk(insufficientStock.asLeft[SkuQuantity], take.asRight[InsufficientStock]))

    assertEquals(actual, Option((NonEmptyChunk(insufficientStock), taken)))
  }

}
