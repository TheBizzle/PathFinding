package tester.testcluster

import tester.Testable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

trait TestCluster[T <: Testable] {
    def getTestsToRun(testNums: List[Int]) : List[TestFunction[T]]
    def getSize : Int
}