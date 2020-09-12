package modesofcomposition


// import cats.data.NonEmptyChain
// import cats.{Parallel, data, effect, implicits, mtl}
// import cats.effect.{Sync, implicits}
// import cats.mtl.implicits

object OrderProcessor {

  //resolveOrderMsg has been broken down into named parts to help understand the pieces of the computation

  def resolveOrderMsg[F[_]: Sync: Parallel: SkuLookup: CustomerLookup](msg: OrderMsg): F[CustomerOrder] =
    msg match { case OrderMsg(custIdStr, items) =>

      val resolveCustomer: F[Customer] =
        for {
          cust_eith <- F.resolveCustomerId(custIdStr)
          cust <- errorValueFromEither[F](cust_eith)
        } yield cust

      val resolveSkuQuantity: ((String, Int)) => F[SkuQuantity] = { case (name, quantity) =>
        for {
          sku_eith <- F.resolveSku(name)
          sku <- errorValueFromEither[F](sku_eith)
          q <- PosInt.fromF[F](quantity)
        } yield SkuQuantity(sku, q)
      }

      val resolveSkus: F[NonEmptyChain[SkuQuantity]] = items.parTraverse(resolveSkuQuantity)

      //applicative composition
      (
        resolveCustomer,
        resolveSkus,
        ).parMapN(CustomerOrder(_, _))
    }
}

