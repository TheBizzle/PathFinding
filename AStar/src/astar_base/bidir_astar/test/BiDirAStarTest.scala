import astar_base.bidir_astar.BiDirAStar
import pathfinding.test.PathingTestCluster
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.TestingCore

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/11
 * Time: 2:48 PM
 */

PathingTestCluster.setThingToTest(BiDirAStar.asInstanceOf[PathFinder[StepData]])
TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 39, RunTest), TestCriteriaToggleFlag(Talkative)), PathingTestCluster)