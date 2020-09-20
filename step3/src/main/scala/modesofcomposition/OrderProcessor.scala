package modesofcomposition

import scala.collection.immutable.SortedSet

import io.chrisdavenport.cats.effect.time.JavaTime
import java.util.UUID

import zio.{ZIO, clock}
import clock.Clock

object OrderProcessor {

  def processCustomerOrder(order: CustomerOrder): ZIO[Clock with Publish, AppError, Unit] = {

    val nonAvailableSkus: List[Sku] = order.items.toList.collect {
      case SkuQuantity(sku, _) if sku.nonAvailableRegions.contains(order.customer.region) => sku 
    }

    if (nonAvailableSkus.isEmpty)
      processAvailableOrder(order)
    else {
      for {
        instant <- clock.instant 
        unav = Unavailable(NonEmptySet.fromSetUnsafe(SortedSet.from(nonAvailableSkus.iterator)), order, instant)
        _ <- Publish.publish(Topic.Unavailable, unav.asJson.toString.getBytes)
      } yield ()

      
    }
  }

  //this is a no-op in step3
  //def processAvailableOrder(order: CustomerOrder): ZIO[Uuids with Inventory with Publish, AppError, Unit] = ZIO.unit
  def processAvailableOrder(order: CustomerOrder): ZIO[Any, AppError, Unit] = ZIO.unit
}

