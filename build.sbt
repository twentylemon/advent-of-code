lazy val advent = (project in file("."))
  .settings(
    scalaVersion := "3.2.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-collections-core" % "0.9.5"
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1"
    ),
    libraryDependencies ++= Seq(
      "com.github.vagmcs" %% "optimus" % "3.4.3",
      "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.3"
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / parallelExecution := false,
    Test / logBuffered := false
  )
