package modesofcomposition

import cats.effect.concurrent.Ref
import scala.collection.mutable
import scala.concurrent.duration.TimeUnit

import zio.{Has, ZLayer, ULayer, URLayer, ZIO, UIO, URIO, ZEnv, Tag}

trait TestSupport {

  val seed = new UuidSeed(Array(1, 2, 3, 4))
  val rabbitCode = "Rabbit"
  val hippoCode = "Hippo"
  val koalaCode = "Koala"
  val toyRabbit = Sku(rabbitCode)
  val toyHippo = Sku(hippoCode)
  val toyKoala = Sku(koalaCode, Set(CustomerRegion.USCanada))
  val skus = Chain(toyRabbit, toyHippo, toyKoala)
  val skuMap = Map.from(skus.map(sku => sku.code -> sku).iterator)

  val initialStock = Map(
    toyRabbit -> NatInt(500),
    toyHippo -> NatInt(100),
    toyKoala -> NatInt(200),
  )

  val ausCustomerIdStr = "12345"
  val ausCustomer = new Customer(ausCustomerIdStr, CustomerRegion.Australia)
  val usCustomerIdStr = "67890"
  val usCustomer = new Customer(usCustomerIdStr, CustomerRegion.USCanada)
  val customerMap = Map(
    ausCustomerIdStr -> ausCustomer,
    usCustomerIdStr -> usCustomer,
  )

  val currMillis = 1577797200000L

  def fromJsonBytes[T: Decoder](bytes: Array[Byte]) = {
    io.circe.parser.decode[T](new String(bytes))
  }

  import java.time.{ DateTimeException, Instant, OffsetDateTime, ZoneId }
  import java.util.concurrent.TimeUnit
  import zio.duration.Duration
  val zioTestClockLayer = ZLayer.succeed(new zio.clock.Clock.Service {
      def currentTime(unit: TimeUnit): UIO[Long] = ZIO.succeed(currMillis)
      def currentDateTime: zio.IO[DateTimeException, OffsetDateTime] = ???
      def nanoTime: UIO[Long] = ???
      def sleep(duration: Duration): UIO[Unit] = ZIO.unit
  })

  def orderJson(customerIdStr: String, hippoQty: Int, rabbitQty: Int) = {
    val rabbitStr = (rabbitQty > 0).valueOrZero(s"""["$rabbitCode", $rabbitQty]""")
    val hippoStr = (hippoQty > 0).valueOrZero(s"""["$hippoCode", $hippoQty]""")
    s"""{
       |"customerId": "$customerIdStr",
       |"skuQuantities": [${Seq(rabbitStr, hippoStr).mkString(", ")}]
       |}""".stripMargin
      }


  def testSkuLookup(skus: Map[String, Sku]): Has[SkuLookup.Service] =
    Has(new SkuLookup.Service {
      override def resolveSku(s: String): zio.IO[AppError, Sku] = 
        zio.ZIO.fromEither(skus.get(s).toRight(StringError(s"Sku code not found: $s")))
    })

  def testCustomerLookup(customerIds: Map[String, Customer]): Has[CustomerLookup.Service] =
    Has( new CustomerLookup.Service {
      override def resolveCustomerId(customerId: String): zio.IO[AppError, Customer] =
        zio.ZIO.fromEither(customerIds.get(customerId).toRight(StringError(s"Customer code not found: $customerId")))
    })

  def skuLayer(skus: Map[String, Sku]): ULayer[SkuLookup] = ZLayer.succeed(new SkuLookup.Service {
    override def resolveSku(s: String): zio.IO[AppError, Sku] = 
      zio.ZIO.fromEither(skus.get(s).toRight(StringError(s"Sku code not found: $s")))
  })

  def customerLayer(customerIds: Map[String, Customer]): ULayer[CustomerLookup] = ZLayer.succeed(new CustomerLookup.Service {
    override def resolveCustomerId(customerId: String): zio.IO[AppError, Customer] =
      zio.ZIO.fromEither(customerIds.get(customerId).toRight(StringError(s"Customer code not found: $customerId")))
  })

  import TestSupport._
  val inventoryLayerFromTest: URLayer[TestStateRef, Inventory] = ZLayer.fromService( testStateRef => 
    
    new Inventory.Service {
      override def inventoryTake(skuQty: SkuQuantity): zio.IO[InsufficientStock, SkuQuantity] =
        testStateRef.modify(ts => {
          val stockQty = ts.invStock.getOrElse(skuQty.sku, NatInt(0))
          NatInt.from(stockQty - skuQty.quantity) match {
            case Right(remaining) =>
              skuQty.asRight -> ts.copy(invStock = ts.invStock.updated(skuQty.sku, remaining))
            case Left(insuffcientMsg) =>
              InsufficientStock(skuQty, stockQty).asLeft -> ts
        }}).absolve

      override def inventoryPut(skuQty: SkuQuantity): zio.IO[AppError, Unit] =
        testStateRef.update(ts => ts.copy(invStock = ts.invStock.updatedWith(skuQty.sku)(current => current |+| (skuQty.quantity: NatInt).some)))

    }
  )


  val publishLayerFromTest: URLayer[TestStateRef, Publish] = ZLayer.fromService( testStateRef => 
    new Publish.Service {
      def publish(topic: String, msg: Array[Byte]): zio.IO[AppError, Unit] =
        testStateRef.update { ts => ts.copy(publishState = ts.publishState.updatedWith(topic)(_ |+| Chain(msg).some)) }
    }
  )

  def testStateLayer(testState: TestState) = ZLayer.fromEffect(zio.Ref.make(testState))

 
  def testLayers(testState: TestState) = {
    val localTestStateLayer = testStateLayer(testState)

    val publishLayer = localTestStateLayer >+> publishLayerFromTest
    val inventoryLayer = localTestStateLayer >>> inventoryLayerFromTest
 
    val allLayers = skuLayer(skuMap) ++ customerLayer(customerMap) ++ inventoryLayer ++ 
      publishLayer ++ Uuids.live ++ zioTestClockLayer  
  
    allLayers  
  }

  val defaultTestState = TestState(Map.empty[String, Chain[Array[Byte]]], initialStock)
  def defaultLayer = testLayers(defaultTestState)

}

object TestSupport extends TestSupport {

  type TestStateRef = Has[zio.Ref[TestState]]
  case class TestState(publishState: Map[String, Chain[Array[Byte]]], invStock: Map[Sku, NatInt]) {
    def getMessages(topic: String): Chain[Array[Byte]] = publishState(topic)
  }

  object TestState {
    def get: URIO[TestStateRef, TestState] = ZIO.accessM(_.get.get)
    //def getMessages(topic: String): URIO[TestStateRef, Chain[Array[Byte]]] = getTestState.flatMap { ts => UIO(ts.publishState(topic)) }
  }

  type AppServices = Uuids with Inventory with Publish with SkuLookup with CustomerLookup
  type TestServices = AppServices with TestStateRef 

  def run[R <: Has[_], E, A](z: ZIO[R, E, A], testState: TestState)(implicit ev: ZEnv with TestServices <:< R): A =
    run(z, testLayers(testState))


  def run[R <: Has[_], E, A, E1 >: E, R1 <: Has[_]](z: ZIO[R, E, A], layer: ZLayer[ZEnv, E1, R1] = defaultLayer)(implicit ev: ZEnv with R1 <:< R, tagged: Tag[R1]): A =
    zio.Runtime.default.unsafeRun(z.provideCustomLayer(layer))

}

