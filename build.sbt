lazy val advent = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion := "3.3.1",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-collections-core" % "0.9.8"
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6"
    ),
    libraryDependencies ++= Seq(
      "com.github.vagmcs" %% "optimus" % "3.4.3",
      "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.3"
    ),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / parallelExecution := false,
    Test / logBuffered := false
  )
