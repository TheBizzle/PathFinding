package astar_base.bidir_astar

import tester.{ criteria, TestCriteriaDialect }, criteria._, TestCriteriaDialect._

import pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/11
 * Time: 2:48 PM
 */

object BiDirAStarTest extends TestScript {
  run(1 >&> 39 && Talkative, BiDirAStar)
}
