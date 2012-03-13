package astar_base.astar

import pathfinding.testcluster.PathingTestCluster
import pathfinding.testscript.TestScript
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.TestingCore

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:37 PM
 */

object AStarQuiet extends TestScript {
  TestingCore(List[TestCriteria[_]]((1, 39, RunTest)), AStar.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
}
