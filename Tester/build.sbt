name := "PathFinding"

version := "0.7"

scalaVersion := "2.10.0"

artifactName := { (_, _, _) => "Tester.jar" }

scalaSource in Compile <<= baseDirectory(_ / "src" / "main")

scalaSource in Test <<= baseDirectory(_ / "src" / "test")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1"
)

mainClass in Compile := None
