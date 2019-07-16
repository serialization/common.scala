name := "ogss.common.scala"

version := "0.1"

scalaVersion := "2.12.8"

javacOptions ++= Seq("-encoding", "UTF-8")

compileOrder := CompileOrder.JavaThenScala

exportJars := true

Compile/packageBin/artifactPath := baseDirectory.value / "ogss.common.scala.jar"

