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

    val dummy = Array[String]()
    val startString = "Start %s test:\n"
    val endString = "\nEnd %s test."

    List((AStarDependencyTest, "dependency"), (AStarQuiet, "AStar"), (BiDirQuiet, "BiDir")).foreach {
      case (test, name) =>
          println(startString.format(name))
          test.main(dummy)
          println(endString.format(name))
    }

}
