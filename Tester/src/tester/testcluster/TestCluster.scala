package tester.testcluster

import testfunction.{TestFunctionFactory, TestFunction}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

trait TestCluster[T <: TestFunction[_, U, _, _], U <: TestSubject] extends TestFunctionFactory[T, U] {
    protected val testFunctionRegex = "TestMapString([0-9]+)(F?)"
    def getTestsToRun(testNums: List[Int]) : List[T]
    def getSize : Int
}