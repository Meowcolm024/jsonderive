package json

import util.Helper._
import io.circe.{Json, Encoder, Decoder}
import io.circe.syntax._

import scala.deriving._
import scala.compiletime._

trait ToJson[T]:
  def toJson(self: T): Json

object ToJson:
  inline given derived[T](using m: Mirror.Of[T]): ToJson[T] =
    lazy val elemInstances = summonAsList[m.MirroredElemTypes, ToJson]
    lazy val elemLabels = constValueTuple[m.MirroredElemLabels]

    inline m match
      case s: Mirror.SumOf[T] =>
        new ToJson[T]:
          def toJson(self: T): Json =
            val ord = s.ordinal(self)
            val tag = elemLabels.productIterator
              .map(tag => tag.asInstanceOf[String])
              .toList(ord)
            val body = elemInstances(ord)
              .asInstanceOf[ToJson[T]]
              .toJson(self)
            Json.obj(tag -> body)

      case p: Mirror.ProductOf[T] =>
        new ToJson[T]:
          def toJson(self: T): Json =
            val elems = self
              .asInstanceOf[Product]
              .productIterator
              .zip(elemInstances.iterator)
              .map((v, coder) => coder.asInstanceOf[ToJson[Any]].toJson(v))
              .zip(elemLabels.productIterator)
              .map((obj, field) => field.asInstanceOf[String] -> obj)
              .toList
            Json.fromFields(elems)

  def toJson[T](value: T)(using c: ToJson[T]): Json = c.toJson(value)

  given ToJson[Boolean] with
    def toJson(self: Boolean): Json =
      if self then Json.True else Json.False

  given ToJson[Int] with
    def toJson(self: Int): Json = Json.fromInt(self)

  given ToJson[String] with
    def toJson(self: String): Json = Json.fromString(self)

  given [T: ToJson]: ToJson[List[T]] with
    def toJson(self: List[T]): Json = Json.fromValues(self.map(ToJson.toJson))
