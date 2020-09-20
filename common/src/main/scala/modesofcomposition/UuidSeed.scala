package modesofcomposition

import java.util.UUID
import zio.{ZIO, UIO, URIO, ZLayer, URLayer, Ref}
import zio.random.Random

/** A 256bit immutable pseudo-random generator seed.
 *
 * A 256bit seed is the smallest seed required (on average) to generate a full random distribution through the 128bit
 * space of UUIDs.  */
final case class UuidSeed(seeds: Array[Long]) {
  require(seeds.length == 4)
}

object UuidSeed {
  //https://en.wikipedia.org/wiki/Linear_congruential_generator
  def permute(l: Long) = l * 6364136223846793005L + 1442695040888963407L

  def next(curr: UuidSeed) = UuidSeed(curr.seeds.map(permute))

  def uuid(curr: UuidSeed) = new UUID(curr.seeds(0) ^ curr.seeds(1), curr.seeds(2) ^ curr.seeds(3))
}

object Uuids {
  trait Service {

    def initSeed(seed: UuidSeed): UIO[Unit]

    def getSeed: UIO[UuidSeed]

    def newSeed: UIO[UuidSeed]

    def nextUuid: UIO[UUID]
  }
    
  def initSeed(seed: UuidSeed): URIO[Uuids, Unit] = ZIO.accessM(_.get.initSeed(seed))

  def newSeed: URIO[Uuids, UuidSeed] = ZIO.accessM(_.get.newSeed)

  def nextUuid: URIO[Uuids, UUID] = ZIO.accessM(_.get.nextUuid)

  val live: URLayer[Random, Uuids] = ZLayer.fromServiceM { random =>
    for { 
      ref <- Ref.make(UuidSeed(Array(1L, 2L, 3L, 4L)))
      svc = new Service {
        def initSeed(seed: UuidSeed) = ref.set(seed)      
        def getSeed: UIO[UuidSeed] = ref.get
        def newSeed: UIO[UuidSeed] =
          for {
            seed <- ZIO.foreach(1 to 4)(_ => random.nextLong).map { coll => UuidSeed(coll.toArray[Long]) }
            _ <- ref.set(seed)
          } yield seed

        def nextUuid: UIO[UUID] = 
          for {
            seed <- ref.get
            uuid = UuidSeed.uuid(seed)
            _ <- ref.set(UuidSeed.next(seed))
          } yield uuid
      }
    } yield svc
  }
}



  //   trait Service {
    //   def newSeed: UIO[UuidSeed] = F.delay(UuidSeed(Array.fill(4)(scala.util.Random.nextLong())))

    //   def nextUuid[F[_]: Functor: UuidRef]: F[UUID] = F.getAndUpdate(_.next).map(_.uuid)
  // }

