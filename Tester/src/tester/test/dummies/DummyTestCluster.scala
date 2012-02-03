package tester.test.dummies

import java.lang.reflect.Field
import tester.testcluster.{TestSubject, Testable, TestCluster}
import tester.testcluster.testfunction.TestFunction
import tester.testanalyzer.{ExecutionStatus, TestAnalysisFlagBundle}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 8:23 PM
 */

object DummyTestCluster extends TestCluster[TestFunction[Testable, TestSubject, ExecutionStatus, TestAnalysisFlagBundle], TestSubject] {
    def getSize = 10
    def getTestsToRun(testNums: List[Int]) : List[TestFunction[Testable, TestSubject, ExecutionStatus, TestAnalysisFlagBundle]] = { null }
    protected def generateTestFunction(fieldData: (Field, String), regex: String) = null
    protected def construct(subject: TestSubject, testNumber: Int, shouldPass: Boolean) = null
}
