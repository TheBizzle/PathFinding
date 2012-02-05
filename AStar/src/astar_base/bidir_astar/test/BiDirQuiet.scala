package astar_base.bidir_astar.test

import astar_base.bidir_astar.BiDirAStar
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.TestingCore
import pathfinding.testcluster.PathingTestCluster
import pathfinding.test.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:35 PM
 */

object BiDirQuiet extends TestScript {
    TestingCore(List[TestCriteria[_]]((1, 39, RunTest)), BiDirAStar.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
}
