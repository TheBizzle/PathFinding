package astar

import
  tester.{ suite, TesterSuiteReporter },
    suite.SuiteReporter

import
  datastructure.DataStructureSuiteReporter

import
  pathfinding.PathFindingSuiteReporter

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:00 PM
 */

object AStarSuiteReporter extends SuiteReporter {
  val reporterSet = Set(AStarSuiteReporter, DataStructureSuiteReporter, PathFindingSuiteReporter, TesterSuiteReporter)
}
