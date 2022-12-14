val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "jsonderive",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-effect" % "3.3.12",
      "io.circe" %% "circe-core" % "0.14.2",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
