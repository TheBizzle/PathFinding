package astar_base

import astar.AStarQuiet
import bidir_astar.BiDirQuiet
import pathfinding.testscript.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/14/12
 * Time: 10:16 PM
 */

object OmniTest extends TestScript {

    val tests = List((AStarDependencyTest, "dependency"), (AStarQuiet, "AStar"), (BiDirQuiet, "BiDir"))

    tests foreach {
      case (test, name) =>
          println("Start %s test:\n".format(name))
          test.main(Array[String]())
          println("\nEnd %s test.".format(name))
    }

}
