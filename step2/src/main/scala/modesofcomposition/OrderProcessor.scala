package modesofcomposition


// import cats.data.NonEmptyChain
// import cats.{Parallel, data, effect, implicits, mtl}
// import cats.effect.{Sync, implicits}
// import cats.mtl.implicits

import zio.{RIO, ZIO}

object OrderProcessor {

  //resolveOrderMsg has been broken down into named parts to help understand the pieces of the computation

  def resolveOrderMsg(msg: OrderMsg): ZIO[SkuLookup with CustomerLookup, AppError, CustomerOrder] = {

    val resolveSkus = ZIO.foreach(msg.skuQuantities) { item =>
      ZIO.mapN(SkuLookup.resolveSku(item.name), PosInt.toZio(item.quantity))(SkuQuantity(_, _))
    }

    // Could do mapParN here and above but no real benefit here
    ZIO.mapN(CustomerLookup.resolveCustomerId(msg.customerId), resolveSkus)(CustomerOrder(_, _))        
    }
}

