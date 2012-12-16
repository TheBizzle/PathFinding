package astar_base.astar

import tester.{ criteria, TestCriteriaDialect }, criteria._, TestCriteriaDialect._

import pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

object AStarTest extends TestScript {
  run(1 >&> 39 && Talkative, AStar)
}
