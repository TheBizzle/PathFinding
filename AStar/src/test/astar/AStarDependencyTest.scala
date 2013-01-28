package astar

import
  tester.{ criteria, suite, Tester },
    criteria._,
    suite.SuiteReporter

import
  pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:42 PM
 */

object AStarDependencyTest extends TestScript {
  Tester(Seq[TestCriteria](RunBaseTests), baseTests = AStarSuiteReporter.reporterSet.foldLeft(new SuiteReporter)(_ ++ _).suites)
}
