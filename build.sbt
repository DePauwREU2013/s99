/** Project */
name := "s99"

version := "1.1"

organization := "org.specs2"

scalaVersion := "2.10.1"

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.0", 
  "org.scala-tools.testing" % "test-interface" % "0.5", 
  "org.specs2" %% "specs2-scalaz-core" % "7.0.0",
  "org.specs2" %% "specs2" % "1.14",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.pegdown" % "pegdown" % "1.0.2",
  "junit" % "junit" % "4.7"
)

scalacOptions ++= Seq("-deprecation", "-unchecked")

/** Console */
initialCommands in console := "import org.specs2._"

