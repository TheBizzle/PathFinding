package tester.testcluster

import testfunction.{TestFuncConstructionBundle, TestFunctionFactory, TestFunction}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

trait TestCluster[T <: TestFunction[_, U, _, _, _], U <: TestSubject, V <: TestFuncConstructionBundle] extends TestFunctionFactory[T, U, V] {
    protected val testFunctionRegex = "TestMapString([0-9]+)((_L([0-9]+))?)"
    def getTestsToRun(testNums: List[Int]) : List[T]
    def getSize : Int
}
