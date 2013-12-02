import sbt._
import sbt.Keys._
import sbtassembly.Plugin
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys

object UniicBuild extends Build {
  val ScalaVersion = "2.10.3"

  val projectSettings =
    Defaults.defaultSettings ++
    sbtassembly.Plugin.assemblySettings ++
    Seq(
      name := "uniic",
      version := "0.1",
      scalaVersion := ScalaVersion,

      scalacOptions ++= Seq("-deprecation", "-unchecked"),

      testOptions in Test += Tests.Argument("-oDF"),  // Full stack traces

      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % ScalaVersion,
        "org.scalaequals" %% "scalaequals-core" % "1.2.0",
        "org.mockito" % "mockito-all" % "1.9.5" % "test",
        "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
        "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1" % "test",
        "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
      ),

      EclipseKeys.withSource := true,
      EclipseKeys.eclipseOutput := Some("target/ide")
    )
  
  val project = Project("uniic", file("."), settings = projectSettings)
}
