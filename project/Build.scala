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


/*

  val x = omniTest <<= state map {
    (s: State) =>
      mainClass in (Compile, run) := Option("astar.OmniTest")
      Project.runTask(Keys.run in Compile, s)
  }

  val e = omniTest <<= run { x =>
    (x, scalaVersion) map { (x, y) =>
      mainClass in Compile := Option("astar.OmniTest")
      println(x)
      println(y)
    }
  }

(omniTest in Test) <<= runTask(Test, "astar.OmniTest") dependsOn (compile, copyResources)
mainClass in (Test, omniTest) := Option("astar.OmniTest")

// It works to set `mainClass in Test := Option("astar.OmniTest")` and then call `test:run` in the SBT console.  Surely, there's some way to do better.  I don't know it, though

 */

}
