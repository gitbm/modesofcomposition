package modesofcomposition

import io.circe.parser

//import modesofcomposition.errorValueFromEither

object OrderProcessor {

  def decodeMsg[F[_]: ApplicativeError[*[_], Throwable]](msg: Array[Byte]): F[OrderMsg] = {
    val decoded = parser.decode[OrderMsg](new String(msg))
   
    println("decoded = " + decoded)

    val f_msg = errorValueFromEither(decoded)

    println("msg = " + msg)

    //errorValueFromEither[F](parser.decode[OrderMsg](new String(msg)))

    f_msg
  }

  val decoded = ...
  errorValueFromEither(decoded)
  errorValueFromEither[F](decoded)

}

