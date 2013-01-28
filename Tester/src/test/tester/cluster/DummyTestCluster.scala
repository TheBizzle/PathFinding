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
  override protected def TestFunctionRegex                                                                                         = ""
  override /*none */ def size                                                                                                      = 10
  override /*none */ def getTestsToRun(testNums: Seq[Int])                                                                         = Seq()
  override protected def construct(subject: TestSubject, testNumber: Int, shouldPass: Boolean, bundle: TestFuncConstructionBundle) = null
  override protected def generateTestFunction(fieldData: (Field, String), regex: String)                                           = None
}
