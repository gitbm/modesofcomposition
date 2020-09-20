package modesofcomposition

import zio.{RIO, ZIO}

/** Sku (Shelf Keeping Unit) is a valid code identifying one purchasable product in the store
 *
 * nonAvailableRegions indicate if Skus are not available to purchase by customers in some regions
 * */
case class Sku private (code: String, nonAvailableRegions: Set[CustomerRegion] = Set.empty)

object Sku {
  implicit def order: Order[Sku] = Order.by(_.code)
}

object SkuLookup {
  trait Service {

    def resolveSku(s: String): ZIO[Any, AppError, Sku]
  }

  def resolveSku(s: String): ZIO[SkuLookup, AppError, Sku] = ZIO.accessM(_.get.resolveSku(s))

}
/** Validates a sku code string is a valid Sku */

