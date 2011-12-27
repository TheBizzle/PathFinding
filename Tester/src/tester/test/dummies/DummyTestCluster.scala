package tester.test.dummies

import tester.testcluster.TestCluster

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 8:23 PM
 */

object DummyTestCluster extends TestCluster[Int] {
    def setThingToTest(thing: Int) { null }
    def runTests(testNums: List[Int], isTalkative: Boolean) { null }
    def getSize = 10
}