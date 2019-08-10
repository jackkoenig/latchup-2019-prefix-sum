
name := "prefix-sum-demo"

version := "0.1.0"

scalaVersion := "2.12.8"

libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.1.7"
libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.2.9" % "test"

scalacOptions ++= Seq(
  "-Xsource:2.11"
)

