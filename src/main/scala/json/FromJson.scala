package json

import util.Helper._
import io.circe.Json
import cats.implicits._

import scala.deriving._
import scala.compiletime._

trait FromJson[T]:
  def fromJson(json: Json): Option[T]

object FromJson:
  inline given derived[T](using m: Mirror.Of[T]): FromJson[T] =
    lazy val elemInstances = summonAsList[m.MirroredElemTypes, FromJson]
    lazy val elemLabels = constValueTuple[m.MirroredElemLabels]

    inline m match
      case s: Mirror.SumOf[T] =>
        new FromJson[T]:
          // type tag
          val tags = elemLabels.productIterator
            .map(tag => tag.asInstanceOf[String])
            .toList
          // helper function associate ordinal with json
          def helper(
              tags: List[(String, Int)],
              f: String => Option[Json]
          ): Option[(Int, Json)] = 
            tags match
              case Nil          => None
              case (t, i) :: ts => f(t).fold(helper(ts, f))(j => Some(i -> j))

          def fromJson(json: Json): Option[T] =
            for
              obj <- json.asObject
              if obj.keys.size == 1
              (ord, j) <- helper(tags.zipWithIndex, obj.apply)
              res <- elemInstances(ord).asInstanceOf[FromJson[T]].fromJson(j)
            yield res

      case p: Mirror.ProductOf[T] =>
        new FromJson[T]:
          // field labels
          val fields = elemLabels.productIterator
            .map(_.asInstanceOf[String])
            .toList
          def fromJson(json: Json): Option[T] =
            for
              obj <- json.asObject
              if obj.keys.toSet == fields.toSet
              prd <- fields.traverse(obj.apply)
              out <- elemInstances
                .zip(prd)
                .traverse((inst, x) =>
                  inst.asInstanceOf[FromJson[Any]].fromJson(x)
                )
            yield p.fromProduct(Tuple.fromArray(out.toArray))

  def fromJson[T](t: Json)(using c: FromJson[T]): Option[T] = c.fromJson(t)

  given FromJson[Boolean] with
    def fromJson(json: Json): Option[Boolean] = json.asBoolean

  given FromJson[Int] with
    def fromJson(json: Json): Option[Int] = json.asNumber.flatMap(_.toInt)

  given FromJson[String] with
    def fromJson(json: Json): Option[String] = json.asString

  given [T: FromJson]: FromJson[List[T]] with
    def fromJson(json: Json): Option[List[T]] =
      json.asArray.flatMap(_.toList.traverse(FromJson.fromJson))
