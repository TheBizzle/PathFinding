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
 * Date: 12/9/11
 * Time: 2:48 PM
 */

object BiDirAStarTest extends TestScript {
  run(1 >&> 39 && Talkative, BiDirAStar)
}

object BiDirQuiet extends TestScript {
  run(1 >&> 39, BiDirAStar)
}
