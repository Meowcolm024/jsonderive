package util

import scala.compiletime.{erasedValue, summonInline}

object Helper:
  inline def summonAsList[T <: Tuple, F[_]]: List[F[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[F[t]].asInstanceOf[F[Any]] :: summonAsList[ts, F]
