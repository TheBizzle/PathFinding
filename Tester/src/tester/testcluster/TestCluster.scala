package tester.testcluster

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

trait TestCluster[T] {
    def setThingToTest(thing: T)
    def runTests(testNums: List[Int], isTalkative: Boolean)
    def getSize : Int
}