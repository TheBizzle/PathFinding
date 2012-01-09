import astar_base.astar.AStar
import pathfinding.test.PathingTestCluster
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.{TestingCore}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

PathingTestCluster.setThingToTest(AStar.asInstanceOf[PathFinder[StepData]])
TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 39, RunTest), TestCriteriaToggleFlag(Talkative)), PathingTestCluster)

// Non-talkative variant
//TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 39, RunTest)), PathingTestCluster)