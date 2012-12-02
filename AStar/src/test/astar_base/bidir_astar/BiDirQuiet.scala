package astar_base.bidir_astar

import pathfinding.testscript.TestScript
import tester.criteria._
import tester.testscript.dialect.TestCriteriaDialect._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:35 PM
 */

object BiDirQuiet extends TestScript {
  run(1 >&> 39, BiDirAStar)
}
