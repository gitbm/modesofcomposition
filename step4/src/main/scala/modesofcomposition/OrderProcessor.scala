package modesofcomposition

import io.chrisdavenport.cats.effect.time.JavaTime

import scala.collection.immutable.SortedSet

object OrderProcessor {

  /** Delegates to dispatchElseBackorder to determine whether the order can be dispatched, then publishes
   * the appropriate message. If   */
  def processAvailableOrder[F[_]: Sync: Parallel: Clock: UuidRef: Inventory: Publish]
  (order: CustomerOrder): F[Unit] = {
    dispatchElseBackorder(order).flatMap {
      case Right(disp) =>
        F.publish(Topic.Dispatch, disp.asJson.toString.getBytes)
      case Left((back_order, sku_qs)) =>
        F.publish(Topic.Backorder, back_order.asJson.toString.getBytes)
    }
  }

  /** Key order business logic: try to take all ordered items from inventory. If all are in stock,
   * the order is dispatched. If any have insufficient stock, then the order wont proceed: return all items
   * to inventory and raise a backorder. */
  def dispatchElseBackorder[F[_]: Sync: Parallel: Clock: UuidRef: Inventory](order: CustomerOrder):
  F[Either[(Backorder, Chain[SkuQuantity]), Dispatched]] = {
    for {
      eith_insuff_taken <- order.items.traverse(F.inventoryTake)
      result <- insufficientsAndTaken(eith_insuff_taken) match {
        case None => 
          dispatch(order).map(Right(_))
        case Some((insuffs, taken)) =>
          taken.traverse(F.inventoryPut) >>
          backorder(insuffs, order).map { bk => Left(bk, taken)}
      }
    } yield result
  }

  /** Generate a backorder by calculating the shortfall in stock to satisfy order */
  def backorder[F[_]: Sync: Clock]
  (insufficientStocks: NonEmptyChain[InsufficientStock], order: CustomerOrder):
  F[Backorder] = {
    val needed = insufficientStocks.traverse { insuff => 
      PosInt.fromF[F](insuff.requested.quantity - insuff.available).map(i => insuff.requested.copy(quantity = i) )
    }
    (needed, F.pure(order), JavaTime[F].getInstant).mapN(Backorder.apply)
  }

  /** generate a dispatch combining the order, a timestap and UUID */
  def dispatch[F[_]: Sync: Clock: UuidRef](order: CustomerOrder): F[Dispatched] = {
    // for {
    //   uuid <- UuidSeed.nextUuid[F]
    //   instant <- JavaTime[F].getInstant
    // } yield Dispatched(order, instant, uuid)
    (F.pure(order), JavaTime[F].getInstant, UuidSeed.nextUuid[F]).mapN(Dispatched.apply)
  }

  /** Transform a collection of inventory.take outcomes into details of a possible shortfall:
   * which items had insufficient stock, and which items were actually taken (since they need to be returned) */
  def insufficientsAndTaken(takes: NonEmptyChain[Either[InsufficientStock, SkuQuantity]]):
  Option[(NonEmptyChain[InsufficientStock], Chain[SkuQuantity])] = {
    val (insuffs, sku_qs) = takes.toChain.separate
    NonEmptyChain.fromChain(insuffs).map(_ -> sku_qs)
  }

}

