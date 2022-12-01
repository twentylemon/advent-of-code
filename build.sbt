
lazy val advent = (project in file("."))
  .settings(
    scalaVersion := "3.2.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )
  