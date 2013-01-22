name := "PathFinding"

version := "0.7"

scalaVersion := "2.10.0"

artifactName := { (_, _, _) => "PathFindingCore.jar" }

scalaSource in Compile <<= baseDirectory(_ / "src" / "main")

scalaSource in Test <<= baseDirectory(_ / "src" / "test")

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1",
  "com.chuusai" % "shapeless_2.10" % "1.2.3"
)

mainClass in Compile := None
