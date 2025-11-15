lazy val advent = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion := "3.3.4",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M19",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-collections-core" % "0.9.9"
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.10",
      "io.circe" %% "circe-generic" % "0.14.10",
      "io.circe" %% "circe-parser" % "0.14.10"
    ),
    libraryDependencies ++= Seq(
      "com.github.vagmcs" %% "optimus" % "3.4.5",
      "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.5"
    ),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.1" % Test,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / parallelExecution := false,
    Test / logBuffered := false,
  )
