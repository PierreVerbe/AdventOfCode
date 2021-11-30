ThisBuild / name := "AdventOfCode"
ThisBuild / organization := "com.gitHub"
ThisBuild / version := "1.0"

ThisBuild / scalaVersion := "2.12.8"

// Properties build
lazy val sparkVersion = "3.2.0"
lazy val scalaTestVersion = "3.0.8"

// Apache Spark
val sparkCore = "org.apache.spark" %% "spark-core" % sparkVersion
val sparkSQl = "org.apache.spark" %% "spark-sql" % sparkVersion

// Tests
val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(sparkCore % Provided,
    sparkSQl % Provided),

  libraryDependencies += scalaTest % Test,
)

lazy val root = (project in file("."))
  .settings(commonSettings : _*)
  .settings(
    name := "AdventOfCode Project"
  )
