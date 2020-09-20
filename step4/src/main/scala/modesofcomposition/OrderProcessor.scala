package modesofcomposition

import io.chrisdavenport.cats.effect.time.JavaTime

import scala.collection.immutable.SortedSet

import zio.{ZIO, clock, Chunk, NonEmptyChunk, UIO}
import clock.Clock

object OrderProcessor {

  /** Delegates to dispatchElseBackorder to determine whether the order can be dispatched, then publishes
   * the appropriate message. If   */
  def processAvailableOrder(order: CustomerOrder): ZIO[Clock with Uuids with Inventory with Publish, AppError, Unit] = {
    dispatchElseBackorder(order).flatMap {
      case Right(disp) =>
        Publish.publish(Topic.Dispatch, disp.asJson.toString.getBytes)
      case Left((back_order, sku_qs)) =>
        Publish.publish(Topic.Backorder, back_order.asJson.toString.getBytes)
    }
  }

  /** Key order business logic: try to take all ordered items from inventory. If all are in stock,
   * the order is dispatched. If any have insufficient stock, then the order wont proceed: return all items
   * to inventory and raise a backorder. */
  def dispatchElseBackorder(order: CustomerOrder): ZIO[Clock with Uuids with Inventory, AppError, Either[(Backorder, List[SkuQuantity]), Dispatched]] = {
    for {
      eith_insuff_taken <- ZIO.foreach(order.items)(Inventory.inventoryTake(_).either)
      result <- insufficientsAndTaken(eith_insuff_taken) match {
        case None => 
          dispatch(order).map(Right(_))
        case Some((insuffs, taken)) =>
          ZIO.foreach(taken)(Inventory.inventoryPut) *>
          backorder(insuffs, order).map { bk => Left(bk, taken)}
      }
    } yield result
  }

  /** Generate a backorder by calculating the shortfall in stock to satisfy order */
  def backorder(insufficientStocks: NonEmptyChunk[InsufficientStock], order: CustomerOrder): ZIO[Clock, AppError, Backorder] = {
    val needed = ZIO.foreach(insufficientStocks) { insuff => 
      PosInt.toZio(insuff.requested.quantity - insuff.available).map(i => insuff.requested.copy(quantity = i) )
    }
    ZIO.mapN(needed, ZIO.succeed(order), clock.instant)(Backorder.apply)
  }

  /** generate a dispatch combining the order, a timestap and UUID */
  def dispatch(order: CustomerOrder): ZIO[Clock with Uuids, AppError, Dispatched] = {

    ZIO.mapN(ZIO.succeed(order), clock.instant, Uuids.nextUuid)(Dispatched.apply)
  }

  /** Transform a collection of inventory.take outcomes into details of a possible shortfall:
   * which items had insufficient stock, and which items were actually taken (since they need to be returned) */
  def insufficientsAndTaken(takes: NonEmptyChunk[Either[InsufficientStock, SkuQuantity]]):
  Option[(NonEmptyChunk[InsufficientStock], List[SkuQuantity])] = {
    val (insuffs, sku_qs) = takes.toList.separate
    NonEmptyChunk.fromChunk(Chunk.fromIterable(insuffs)).map(_ -> sku_qs)
  }

}

