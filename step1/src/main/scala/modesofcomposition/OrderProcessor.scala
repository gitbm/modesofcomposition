package modesofcomposition

import io.circe.{parser, Encoder, Decoder, NonEmptySeqDecoder}
import zio._
import scala.util.Success

object OrderProcessor {
  def decodeMsg(msg: Array[Byte]): ZIO[Any, Throwable, OrderMsg] = {
      
    ZIO.fromEither(parser.decode[OrderMsg](new String(msg)))
  }

}

