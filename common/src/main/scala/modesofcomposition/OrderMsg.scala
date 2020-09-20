package modesofcomposition

/** An incoming order request that has been parsed but not yet validated */
case class OrderSkuQuantity(name: String, quantity: Int)
case class OrderMsg(customerId: String, skuQuantities: zio.NonEmptyChunk[OrderSkuQuantity])