name := "machine_learning"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.5" % "test",
	"org.scalanlp" %% "breeze" % "0.13.2",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"