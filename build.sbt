lazy val advent = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion := "3.10.0",
    scalacOptions ++= Seq("-language:implicitConversions"),
    // scalacOptions ++= Seq("-rewrite", "-source:future-migration"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.3.0",
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.1.2",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.17",
      "io.circe" %% "circe-generic" % "0.14.17",
      "io.circe" %% "circe-parser" % "0.14.17"
    ),
    libraryDependencies += "com.google.ortools" % "ortools-java" % "9.15.6755",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.20.0" % Test,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / parallelExecution := false,
    Test / logBuffered := false,
    coverageExcludedPackages := "org\\.lemon\\.advent\\.(?!lib(\\.|$)).*;org\\.lemon\\.advent\\.lib\\.Bench",
    strykerMutate := Seq("src/main/scala/org/lemon/advent/lib/**/*.scala", "!**/lib/bench.scala"),
    strykerTestFilter := Seq("org.lemon.advent.lib.*"),
    strykerExcludedMutations := Seq("StringLiteral"),
    strykerConcurrency := 2,
  )

addCommandAlias("libCoverage", "; coverage; testOnly org.lemon.advent.lib.*; coverageReport; coverageOff")
