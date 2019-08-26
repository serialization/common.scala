name := "ogss.common.scala"

version := "0.9"

scalaVersion := "2.12.9"

javacOptions ++= Seq("-encoding", "UTF-8")

compileOrder := CompileOrder.JavaThenScala

exportJars := true

Compile/packageBin/artifactPath := baseDirectory.value / "ogss.common.scala.jar"

