package astar_base.astar.test

import astar_base.astar.AStar
import pathfinding.testcluster.PathingTestCluster
import pathfinding.test.TestScript
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.{TestingCore}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:37 PM
 */

object AStarQuiet extends TestScript {
    TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 39, RunTest)), AStar.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
}