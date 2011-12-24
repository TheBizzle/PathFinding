import astar_base.bidir_astar.BiDirAStar
import pathfinding.tester.criteria._
import pathfinding.tester.TestingCore

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/11
 * Time: 2:48 PM
 */

TestingCore(List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaToggleFlag(Talkative)), BiDirAStar)