package org.bizzle.astar

import
  org.bizzle.tester.{ suite, TesterSuiteReporter },
    suite.SuiteReporter

import
  org.bizzle.datastructure.DataStructureSuiteReporter

import
  org.bizzle.pathfinding.PathFindingSuiteReporter

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:00 PM
 */

object AStarSuiteReporter extends SuiteReporter {
  val reporterSet = Set(AStarSuiteReporter, DataStructureSuiteReporter, PathFindingSuiteReporter, TesterSuiteReporter)
}
