package astar_base.astar

import tester.{ criteria, TestCriteriaDialect }, criteria._, TestCriteriaDialect._

import pathfinding.testscript.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 11:37 PM
 */

object AStarQuiet extends TestScript {
  run(1 >&> 39, AStar)
}
