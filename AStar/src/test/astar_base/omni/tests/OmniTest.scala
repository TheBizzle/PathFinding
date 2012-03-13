package astar_base.omni.tests

import astar_base.astar.AStarTest
import astar_base.bidir_astar.BiDirAStarTest
import astar_base.AStarDependencyTest
import astar_base.omni.OmniTestBase


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/14/12
 * Time: 10:16 PM
 */

object OmniTest extends OmniTestBase {
  run()
  protected def tests = List((AStarDependencyTest, "dependency"), (AStarTest, "AStar"), (BiDirAStarTest, "BiDir"))
}
