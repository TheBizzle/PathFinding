package org.bizzle.astar

import
  org.bizzle.tester.{ criteria, suite, Tester },
    criteria._,
    suite.SuiteReporter

import
  org.bizzle.pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:42 PM
 */

object AStarDependencyTest extends TestScript {
  Tester.runSuites(AStarSuiteReporter.reporterSet.foldLeft(new SuiteReporter)(_ ++ _).suites: _*)
}
