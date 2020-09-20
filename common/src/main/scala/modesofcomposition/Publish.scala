package modesofcomposition

import zio.ZIO

/** Models a topic-oriented message publication service */
object Publish {
  trait Service {

    /** Sends msg bytes to a named topic */
    def publish(topic: String, msg: Array[Byte]): zio.IO[AppError, Unit]
  }

  def publish(topic: String, msg: Array[Byte]): ZIO[Publish, AppError, Unit] =
    ZIO.accessM(_.get.publish(topic, msg))
}

object Topic {
  val Dispatch = "DISPATCH"
  val Backorder = "BACKORDER"
  val Deadletter = "DEADLETTER"
  val Unavailable = "UNAVAILABLE"
}
