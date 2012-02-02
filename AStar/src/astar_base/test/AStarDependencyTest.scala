package astar_base.test

import suitereporter.AStarSuiteCoagulator
import tester.criteria.{RunBaseTests, TestCriteria}
import pathfinding.test.TestScript
import tester.TestingCore

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:42 PM
 */

object  AStarDependencyTest extends TestScript {
    TestingCore(List[TestCriteria[_]](RunBaseTests), baseTests = AStarSuiteCoagulator.coagulate)
}