package modesofcomposition

import zio.NonEmptyChunk
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

import java.time.Instant
import TestSupport._

object OrderProcessTestZ extends DefaultRunnableSpec {

  def spec = suite("Step4")(

    testM("processAvailableOrder - dispatch") {
      val order = CustomerOrder(ausCustomer,
        NonEmptyChunk(SkuQuantity(toyRabbit, refineMV[Positive](1))))
      val expected = Chain(Dispatched(order, Instant.ofEpochMilli(currMillis), seed.uuid)).asRight[io.circe.Error]
      
        for {
          _ <- Uuids.initSeed(seed) *> OrderProcessor.processAvailableOrder(order)
          testState <- TestState.get
          msgs = testState.getMessages(Topic.Dispatch).traverse(fromJsonBytes[Dispatched])
        } yield 
          assert(msgs)(equalTo(expected)) && 
          assert(testState.invStock)(equalTo(initialStock.updated(toyRabbit, NatInt(499))))
      
      }.provideCustomLayer(defaultLayer),

    testM("processAvailableOrder - backorder") {
      val lowHippoStock = initialStock.updated(toyHippo, NatInt(1))

      val order = CustomerOrder(ausCustomer, NonEmptyChunk(
        SkuQuantity(toyRabbit, PosInt(1)),
        SkuQuantity(toyHippo, PosInt(2)),
      ))   
      val expected = Chain(Backorder(NonEmptyChunk(
        SkuQuantity(toyHippo, PosInt(1))), order, Instant.ofEpochMilli(currMillis))).asRight[io.circe.Error]
   
        val test = for {
          _ <- Uuids.initSeed(seed) *> OrderProcessor.processAvailableOrder(order)
          testState <- TestState.get
          msgs = testState.getMessages(Topic.Backorder).traverse(fromJsonBytes[Backorder])
        } yield 
          assert(msgs)(equalTo(expected)) && 
          assert(testState.invStock)(equalTo(lowHippoStock))

        test.provideCustomLayer(testLayers(defaultTestState.copy(invStock = lowHippoStock)))
    },

    testM("backorder") {
      val order = CustomerOrder(ausCustomer, NonEmptyChunk(
        SkuQuantity(toyRabbit, PosInt(1)),
        SkuQuantity(toyHippo, PosInt(2)),
      ))
      val insufficientStocks = NonEmptyChunk(
        InsufficientStock(SkuQuantity(toyHippo, PosInt(2)), NatInt(1)))
   
      for {
        backOrder <- OrderProcessor.backorder(insufficientStocks, order)
      } yield 
        assert(backOrder)( 
               equalTo(Backorder(NonEmptyChunk(SkuQuantity(toyHippo, PosInt(1))), order, 
                                 Instant.ofEpochMilli(currMillis))))
      
    }.provideCustomLayer(defaultLayer),

    testM("dispatch") {
      val order = CustomerOrder(ausCustomer, NonEmptyChunk(
        SkuQuantity(toyRabbit, PosInt(1)),
        SkuQuantity(toyHippo, PosInt(2)),
      ))
      for {
        dispatch <- Uuids.initSeed(seed) *> OrderProcessor.dispatch(order)
      } yield assert(dispatch)(equalTo(Dispatched(order, Instant.ofEpochMilli(currMillis), seed.uuid)))
    }.provideCustomLayer(defaultLayer),

    test("insufficientsAndTaken") {
      val insufficientStock = InsufficientStock(SkuQuantity(toyHippo, PosInt(2)), NatInt(1))
      val take = SkuQuantity(toyKoala, PosInt(1))
      val taken = List(take)
      val insuff = OrderProcessor.insufficientsAndTaken(
                      NonEmptyChunk(insufficientStock.asLeft[SkuQuantity], take.asRight[InsufficientStock]))
      assert(insuff)(equalTo(Option((NonEmptyChunk(insufficientStock), taken))))
    }

  )
}
