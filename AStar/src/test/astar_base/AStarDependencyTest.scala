package astar_base

import tester.{ criteria, Tester }, criteria._


import suitereporter.AStarSuiteCoagulator
import pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:42 PM
 */

object AStarDependencyTest extends TestScript {
  Tester(Seq[TestCriteria](RunBaseTests), baseTests = AStarSuiteCoagulator.coagulate)
}
