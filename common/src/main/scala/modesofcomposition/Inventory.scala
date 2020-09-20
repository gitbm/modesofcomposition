package modesofcomposition

import zio.ZIO

/** Interface to the stores product inventory */
object Inventory {
  trait Service {

    /** Take amount of product represented by skuQty from inventory, or if there is InsufficientStock provide details of it */
    def inventoryTake(skuQty: SkuQuantity): zio.IO[InsufficientStock, SkuQuantity]

    /** Add the amount of product represented by skuQty to inventory */
    def inventoryPut(skuQty: SkuQuantity): zio.IO[AppError, Unit]
  }

    def inventoryTake(skuQty: SkuQuantity): ZIO[Inventory, InsufficientStock, SkuQuantity] = 
      ZIO.accessM(_.get.inventoryTake(skuQty))

    def inventoryPut(skuQty: SkuQuantity): ZIO[Inventory, AppError, Unit] =
      ZIO.accessM(_.get.inventoryPut(skuQty))

}
/** When an inventory take fails, details what was request and what is available. */
case class InsufficientStock(requested: SkuQuantity, available: NatInt) extends AppError {
  require(available < requested.quantity, s"require available $available < requested.quantity ${requested.quantity}")
}
