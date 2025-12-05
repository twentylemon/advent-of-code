lazy val advent = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion := "3.3.4",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.13",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.15",
      "io.circe" %% "circe-generic" % "0.14.15",
      "io.circe" %% "circe-parser" % "0.14.15"
    ),
    libraryDependencies ++= Seq(
      "com.github.vagmcs" %% "optimus" % "3.4.5",
      "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.5"
    ),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.19.0" % Test,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / parallelExecution := false,
    Test / logBuffered := false,
  )
