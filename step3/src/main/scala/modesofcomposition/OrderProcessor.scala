package modesofcomposition

import scala.collection.immutable.SortedSet

import io.chrisdavenport.cats.effect.time.JavaTime
import java.util.UUID

object OrderProcessor {

  def processCustomerOrder[F[_]: Sync: Parallel: Clock: UuidRef: Inventory: Publish](
    order: CustomerOrder): F[Unit] = {

    val nonAvailableSkus: Chain[Sku] = order.items.collect { 
      case SkuQuantity(sku, _) if sku.nonAvailableRegions.contains(order.customer.region) => sku 
    }

    if (nonAvailableSkus.isEmpty)
      processAvailableOrder[F](order)
    else {
      for {
        instant <- JavaTime[F].getInstant
        unav = Unavailable(NonEmptySet.fromSetUnsafe(SortedSet.from(nonAvailableSkus.iterator)), order, instant)
        _ <- F.publish(Topic.Unavailable, unav.asJson.toString.getBytes)
      } yield ()

      
    }
  }

  //this is a no-op in step3
  def processAvailableOrder[F[_] : Functor: Sync: Parallel: Clock: UuidRef: Inventory: Publish]
    (order: CustomerOrder): F[Unit] = F.unit
}

