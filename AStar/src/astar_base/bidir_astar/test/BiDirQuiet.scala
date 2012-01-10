package astar_base.bidir_astar.test

import astar_base.bidir_astar.BiDirAStar
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.TestingCore
import pathfinding.test.{TestScript, PathingTestCluster}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:35 PM
 */

object BiDirQuiet extends TestScript {
    PathingTestCluster.setThingToTest(BiDirAStar.asInstanceOf[PathFinder[StepData]])
    TestingCore(List[TestCriteria[_]]((2, 39, RunTest)), PathingTestCluster)
}