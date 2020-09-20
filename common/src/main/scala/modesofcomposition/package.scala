import cats.effect.concurrent.Ref

import zio.Has

package object modesofcomposition extends RefinedSupport with ErrorValueSupport {

  type SkuLookup = Has[SkuLookup.Service]
  type CustomerLookup = Has[CustomerLookup.Service]
  type Inventory = Has[Inventory.Service]
  type Publish = Has[Publish.Service]

  //type UuidRef[F[_]] = Ref[F, UuidSeed]
  type UuidRef = Has[zio.Ref[UuidSeed]]
  type Uuids = Has[Uuids.Service]

  import zio.NonEmptyChunk, io.circe.{Encoder, Decoder}, scala.util.Success
  implicit final def encodeNonEmptyChunk[A](implicit encodeA: Encoder[A]): ArrayEncoder[NonEmptyChunk[A]] =
    new ArrayEncoder[NonEmptyChunk[A]] {
      final def encodeArray(a: NonEmptyChunk[A]): Vector[Json] = a.toVector.map(encodeA(_))
    }

  implicit final def decodeNonEmptyChunk[A](implicit decodeA: Decoder[A]): Decoder[NonEmptyChunk[A]] =
    Decoder.decodeNonEmptyList[A].emapTry { nel => 
      Success(NonEmptyChunk(nel.head, nel.tail:_*))
    }
}
