import json._

import io.circe.{Json, Encoder, Decoder}
import io.circe.syntax._

class Spec extends munit.FunSuite:

  enum Omg derives ToJson, FromJson, Encoder.AsObject, Decoder:
    case Mono(i: Int)
    case Duo(i: Boolean, j: Omg)
    case Tri(x: String, y: Omg, z: Omg)

  test("simple json encode") {
    import Omg._
    val a = Tri("hello", Mono(1), Duo(false, Mono(3)))
    assertEquals(ToJson.toJson(a), a.asJson)
  }

  test("simple json decode") {
    import Omg._
    val j = Json.obj(
      "Duo" -> Json.obj(
        "i" -> true.asJson,
        "j" -> Json.obj(
          "Mono" -> Json.obj("i" -> 0.asJson)
        )
      )
    )
    val t = Duo(true, Mono(0))
    val fj = FromJson.fromJson[Omg](j)
    val cj = j.as[Omg].toOption
    assert(fj == cj && fj.contains(t) && cj.contains(t))
  }

  test("decode fails") {
    val j = Json.obj("Mono" -> Json.True)
    assert(FromJson.fromJson[Omg](j).isEmpty && j.as[Omg].isLeft)
  }
