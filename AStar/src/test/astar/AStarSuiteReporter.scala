package astar

import
  tester.{ suite, TesterSuiteReporter },
    suite.{ SuiteCoagulator, SuiteReporter }

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

// Pass all of the SuiteReporters of your class—and of your dependencies—to the superclass's constructor!
object AStarSuiteReporter   extends SuiteReporter(Seq())
object AStarSuiteCoagulator extends SuiteCoagulator(AStarSuiteReporter, DataStructureSuiteReporter, PathFindingSuiteReporter, TesterSuiteReporter)
