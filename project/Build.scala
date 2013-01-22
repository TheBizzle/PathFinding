import
  sbt._,
    Keys._

object PathFindingBuild extends Build {

  private val forwardingStr = "test->test;compile->compile"

  lazy val astar = Project(id = "astar", base = file("AStar")).aggregate(core).dependsOn(core % forwardingStr)

  lazy val core = Project(id = "core", base = file("PathFindingCore")).aggregate(datastructure).dependsOn(datastructure % forwardingStr)

  lazy val datastructure = Project(id = "datastructure", base = file("DataStructure")).aggregate(tester).dependsOn(tester % forwardingStr)

  lazy val tester = Project(id = "tester", base = file("Tester"))

  lazy val astarTest  = TaskKey[Unit]("astar",       "Runs the AStar tests")
  lazy val astarQuiet = TaskKey[Unit]("astar-quiet", "Runs the AStar tests (quietly)")
  lazy val bidirTest  = TaskKey[Unit]("bidir",       "Runs the BiDirAStar tests")
  lazy val bidirQuiet = TaskKey[Unit]("bidir-quiet", "Runs the BiDirAStar tests (quietly)")
  lazy val omniTest   = TaskKey[Unit]("omni",        "Runs all tests")
  lazy val omniQuiet  = TaskKey[Unit]("omni-quiet",  "Runs all tests (quietly)")

}
