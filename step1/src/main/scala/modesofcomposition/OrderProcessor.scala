package modesofcomposition

object OrderProcessor {

  def decodeMsg[F[_]: ApplicativeError[*[_], Throwable]](msg: Array[Byte]): F[OrderMsg] =
    ???

}

