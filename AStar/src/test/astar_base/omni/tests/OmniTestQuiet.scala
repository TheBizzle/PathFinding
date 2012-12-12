package astar_base.omni.tests

import astar_base.astar.AStarQuiet
import astar_base.bidir_astar.BiDirQuiet
import astar_base.omni.OmniTestBase
import astar_base.AStarDependencyTest

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/15/12
 * Time: 11:47 PM
 */

object OmniTestQuiet extends OmniTestBase {
  run()
  protected def tests = Seq((AStarDependencyTest, "dependency"), (AStarQuiet, "AStar"), (BiDirQuiet, "BiDir"))
}
