package org.bizzle.astar

import
  org.bizzle.tester.{ criteria, TestCriteriaDialect },
    criteria._,
    TestCriteriaDialect._

import
  org.bizzle.pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

object AStarTest extends TestScript {
  run(1 >&> 39 && Talkative, AStar)
}

object AStarQuiet extends TestScript {
  run(1 >&> 39, AStar)
}
