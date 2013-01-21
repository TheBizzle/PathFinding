package pathfinding

import
  pathfinding.pathingmap.PathingMapSpec

import
  tester.suite.SuiteReporter

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:57 PM
 */

object PathFindingSuiteReporter extends SuiteReporter(Seq(new PathingMapSpec()))
