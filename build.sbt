name := "steelmillslab"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.glassfish" % "javax.json" % "1.1")

mainClass in Compile := Some("org.xcsp.modeler.Compiler")

enablePlugins(JavaAppPackaging)