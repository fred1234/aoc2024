val scala3Version = "3.5.2"

val MUnitFramework = new TestFramework("munit.Framework")

lazy val root = project
  .in(file("."))
  .settings(
    name := "2024",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1",
    Test / testOptions += Tests.Argument("junitxml")
  )
