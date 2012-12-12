package astar_base

import suitereporter.AStarSuiteCoagulator
import pathfinding.testscript.TestScript
import tester.TestingCore
import tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:42 PM
 */

object AStarDependencyTest extends TestScript {
  TestingCore(Seq[TestCriteria](RunBaseTests), baseTests = AStarSuiteCoagulator.coagulate)
}
