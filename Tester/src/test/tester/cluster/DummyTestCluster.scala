package tester.cluster

import
  java.lang.reflect.Field

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 8:23 PM
 */

object DummyTestCluster extends TestCluster[TestFunction[Testable, TestSubject, ExecutionStatus, TestAnalysisFlagBundle, TestAnalysisResultBundle], TestSubject, TestFuncConstructionBundle] {
  override protected def TestFunctionRegex = ""
  override           def getSize = 10
  override protected def construct(subject: TestSubject, testNumber: Int, shouldPass: Boolean, bundle: TestFuncConstructionBundle) = null
  override protected def generateTestFunction(fieldData: (Field, String), regex: String) = null
  override           def getTestsToRun(testNums: Seq[Int]) : Seq[TestFunction[Testable, TestSubject, ExecutionStatus, TestAnalysisFlagBundle, TestAnalysisResultBundle]] = null
}
