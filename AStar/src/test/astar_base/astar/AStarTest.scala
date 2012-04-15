package astar_base.astar

import pathfinding.testscript.TestScript
import tester.criteria.Talkative
import tester.testscript.dialect.TestCriteriaDialect._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

object AStarTest extends TestScript {
  run(1 >&> 39 && Talkative, AStar)
}
