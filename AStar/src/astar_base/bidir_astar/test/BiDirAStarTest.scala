package astar_base.bidir_astar.test

import astar_base.bidir_astar.BiDirAStar
import pathfinding.test.{TestScript, PathingTestCluster}
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.TestingCore

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/11
 * Time: 2:48 PM
 */

object BiDirAStarTest extends TestScript {
    PathingTestCluster.setThingToTest(BiDirAStar.asInstanceOf[PathFinder[StepData]])
    TestingCore(List[TestCriteria[_]]((1, 39, RunTest), Talkative), PathingTestCluster)
}
