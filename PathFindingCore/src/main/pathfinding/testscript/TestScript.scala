package pathfinding.testscript

import tester.criteria.{TestCriteria, TestCriteriaValueTuple, TestCriteriaRangeTuple, TestRunningnessFlag}
import tester.criteria.parser.TestCriteriaParser
import pathfinding.testcluster.PathingTestCluster
import tester.TestingCore
import pathfinding.{StepData, PathFinder}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/10/12
 * Time: 12:09 AM
 */

abstract class TestScript extends App {
  //@ This will need work
  implicit def strToCriteriaList(s: String) : List[TestCriteria[_]] = {
    TestCriteriaParser.parseAll(TestCriteriaParser.criteria, s).get
  }
  def run(criteria: List[TestCriteria[_]], pf: PathFinder[_ <: StepData]) {
    TestingCore(criteria, pf.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
  }
}
