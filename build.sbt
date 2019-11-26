name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "2.4.1",
  "org.apache.spark" %% "spark-sql" % "2.4.1"
)

//libraryDependencies += "org.scalastyle" % "scalastyle-maven-plugin" % "1.0.0"
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")