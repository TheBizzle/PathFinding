package astar_base.astar.test

import astar_base.astar.AStar
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.{TestingCore}
import pathfinding.testcluster.PathingTestCluster
import pathfinding.test.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

object AStarTest extends TestScript {
    TestingCore(List[TestCriteria[_]]((17, RunTest), TestCriteriaToggleFlag(Talkative)), AStar.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
}
