val scala3Version = "3.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "GuessTheNumberFp",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "dev.zio" %% "zio-test" % "1.0.12"
    )
  )
