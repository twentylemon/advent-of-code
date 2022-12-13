
val circeVersion = "0.14.1"

lazy val advent = (project in file("."))
  .settings(
    scalaVersion := "3.2.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    )
  )
