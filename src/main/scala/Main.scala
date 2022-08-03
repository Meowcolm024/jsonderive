import json._
import io.circe.{Json, Encoder, Decoder}
import io.circe.syntax._
import cats.effect._

object Main extends IOApp.Simple:
  enum Maybe derives ToJson, FromJson, Encoder.AsObject, Decoder:
    case Nothing() // fails with singleton object (while circe cannot compile)
    case Just(value: Int)

  def run: IO[Unit] =
    val eg = Maybe.Just(114514)
    for
      _ <- IO.println(s"Encode $eg to json")
      _ <- IO.println("> With circe's Encoder: ")
      _ <- IO.println(eg.asJson)
      _ <- IO.println("> With our derived ToJson: ")
      _ <- IO.println(ToJson.toJson(eg))
    yield ()
