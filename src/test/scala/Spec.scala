import json._
import io.circe.{Json, Encoder, Decoder}
import io.circe.syntax._

class Spec extends munit.FunSuite:

  enum Omg derives ToJson, FromJson, Encoder.AsObject, Decoder:
    case Mono(i: List[Option[Int]])
    case Duo(i: Boolean, j: Omg)
    case Tri(x: String, y: Omg, z: Omg)

  case class Box(v: Option[String])
      derives ToJson,
        FromJson,
        Encoder.AsObject,
        Decoder

  test("simple json encode") {
    import Omg._
    val a = Tri(
      "hello",
      Mono(List(Some(1), Some(2))),
      Duo(false, Mono(List(None, Some(3))))
    )
    assertEquals(ToJson.toJson(a), a.asJson)
  }

  test("simple json decode") {
    import Omg._
    val j = Json.obj(
      "Duo" -> Json.obj(
        "i" -> true.asJson,
        "j" -> Json.obj(
          "Mono" -> Json.obj(
            "i" -> Json.arr(Option(0).asJson)
          )
        )
      )
    )
    val t = Duo(true, Mono(Some(0) :: Nil))
    val fj = FromJson.fromJson[Omg](j)
    val cj = j.as[Omg].toOption
    assert(fj.contains(t) && cj.contains(t))
    assertEquals(fj, cj)
  }

  test("decode fails") {
    val j = Json.obj("Mono" -> Json.True)
    val k = Json.obj("v" -> 233.asJson)
    val p = Json.obj("v" -> Json.Null)
    assert(FromJson.fromJson[Omg](j).isEmpty && j.as[Omg].isLeft)
    assert(FromJson.fromJson[Box](k).isEmpty && k.as[Box].isLeft)
    assert(FromJson.fromJson[Omg](k).isEmpty && k.as[Omg].isLeft)
  }

  test("option encode") {
    val b = Box(Some("hi"))
    val c = Option.empty[Int]
    assertEquals(ToJson.toJson(b), b.asJson)
    assertEquals(ToJson.toJson(c), c.asJson)
  }

  test("option decode") {
    val j = Json.obj("v" -> "kamisato".asJson)
    val k = Json.obj("v" -> Json.Null)
    assertEquals(FromJson.fromJson[Box](j), j.as[Box].toOption)
    assertEquals(FromJson.fromJson[Box](k), k.as[Box].toOption)
  }

  test("list encode and decode") {
    val ls = List(1, 2, 3)
    val os = ls.asJson
    assertEquals(ToJson.toJson(ls), os)
    assertEquals(FromJson.fromJson[List[Int]](os), Some(ls))
  }
