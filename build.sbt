name := "FirstHomework"

version := "0.1"

scalaVersion := "2.13.3"

val doobieVersion = "0.9.0"
val catsScalacheckVersion = "0.2.0"

libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
  "org.tpolecat" %% "doobie-scalatest" % doobieVersion % Test
)