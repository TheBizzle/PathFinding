package tester.testcluster

import testfunction.{TestFuncConstructionBundle, TestFunctionFactory, TestFunction}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

trait TestCluster[T <: TestFunction[_, U, _, _, _], U <: TestSubject, V <: TestFuncConstructionBundle] extends TestFunctionFactory[T, U, V] {
  protected def TestFunctionRegex : String
  def getTestsToRun(testNums: Seq[Int]) : Seq[T]
  def getSize : Int
}
