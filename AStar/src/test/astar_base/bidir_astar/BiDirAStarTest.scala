package astar_base.bidir_astar

import pathfinding.testscript.TestScript
import tester.criteria._
import tester.testscript.dialect.TestCriteriaDialect._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/11
 * Time: 2:48 PM
 */

object BiDirAStarTest extends TestScript {
  run(1 >&> 39 && Talkative, BiDirAStar)
}
