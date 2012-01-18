package tester.test.dummies

import tester.testcluster.{TestFunction, TestCluster}
import tester.Testable


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 8:23 PM
 */

object DummyTestCluster extends TestCluster[Testable] {
    def getTestsToRun(testNums: List[Int]) : List[TestFunction[Testable]] = { null }
    def getSize = 10
}