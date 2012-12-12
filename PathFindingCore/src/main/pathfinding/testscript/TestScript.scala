package pathfinding.testscript

import pathfinding.testcluster.PathingTestCluster
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import tester.TestingCore

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/10/12
 * Time: 12:09 AM
 */

abstract class TestScript extends App {
  def run(criteria: Seq[TestCriteria], pf: PathFinder[_ <: StepData]) {
    TestingCore(criteria, pf.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
  }
}
