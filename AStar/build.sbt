name := "AStar"

version := "1.0"

scalaVersion := "2.10.1"

artifactName := { (_, _, _) => "AStar.jar" }

scalaSource in Compile <<= baseDirectory(_ / "src" / "main")

scalaSource in Test <<= baseDirectory(_ / "src" / "test")

resolvers += "Typesafe Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2-SNAPSHOT",
  "org.scalatest" % "scalatest_2.10" % "1.9.1"
)

mainClass in Compile := None

PathFindingBuild.tasks
