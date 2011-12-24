import astar_base.astar.AStar
import pathfinding.tester.criteria._
import pathfinding.tester.{TestingCore}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaToggleFlag(Talkative)), AStar)