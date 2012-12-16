package astar_base.bidir_astar

import tester.{ criteria, TestCriteriaDialect }, criteria._, TestCriteriaDialect._

import pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:35 PM
 */

object BiDirQuiet extends TestScript {
  run(1 >&> 39, BiDirAStar)
}
