package tester.testcluster

import tester.testanalyzer.TestAnalysisFlagBundle
import tester.{TestSubject, ExecutionStatus, Testable}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

// Constructor is passed things that are statically known about the TestFunction
// apply() is passed things that dynamically affect the outcome of the function
abstract class TestFunction[T <: Testable, Subject <: TestSubject, Status <: ExecutionStatus, AnalysisFlags <: TestAnalysisFlagBundle]
                           (subject: Subject, analysisFunction: (Status, AnalysisFlags) => Boolean, testNumber: Int, shouldPass: Boolean) extends Function2[T, TestFuncFlagBundle, Boolean] {

    val testSubject = subject
    val testNum = testNumber
    val shouldSucceed = shouldPass
    protected val analysisFunc = analysisFunction

    def apply(testable: T, testFlags: TestFuncFlagBundle) : Boolean
    protected def extractAnalysisFlags(testFlags: TestFuncFlagBundle) : AnalysisFlags

}