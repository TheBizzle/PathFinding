import astar_base.astar.AStar
import pathfinding.test.PathingTestCluster
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.{TestingCore}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:37 PM
 */
PathingTestCluster.setThingToTest(AStar.asInstanceOf[PathFinder[StepData]])
TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 39, RunTest)), PathingTestCluster)