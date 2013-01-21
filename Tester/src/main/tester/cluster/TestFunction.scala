package tester.cluster

import
  java.lang.reflect.Field

import
  tester.criteria.{ Talkative, TestToggleFlag, ToggleFlagManager }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

abstract class TestFunction[T <: Testable, Subject <: TestSubject, Status <: ExecutionStatus, AnalysisFlags <: TestAnalysisFlagBundle, AnalysisResult <: TestAnalysisResultBundle]
                           (subject: Subject, analysisFunction: (Status, AnalysisFlags) => AnalysisResult, testNumber: Int, shouldPass: Boolean) extends ((T, TestFuncFlagBundle) => Boolean) {

  val testNum = testNumber
  val shouldSucceed = shouldPass
  protected val testSubject = subject
  protected val analysisFunc = analysisFunction

  override def apply(testable: T, testFlags: TestFuncFlagBundle) : Boolean
  protected def extractAnalysisFlags(testFlags: TestFuncFlagBundle) : AnalysisFlags

}

abstract class TestFuncConstructionBundle

class TestFuncFlagBundle(inToggles: Seq[TestToggleFlag], extras: Seq[TestToggleFlag] = Seq())(implicit passItOn: Seq[TestToggleFlag] = extras ++ Seq(Talkative))
  extends ToggleFlagManager(inToggles, passItOn)

trait TestFunctionFactory[T <: TestFunction[_, U, _, _, _], U <: TestSubject, V <: TestFuncConstructionBundle] {

  self: TestCluster[T, U, V] =>

  // Essentially, uses reflection to find to find all T-type fields of PathingTestCluster
  def generateTests : Seq[T] = {
    val genFunc = generateTestFunction(_: (Field, String), regex = TestFunctionRegex)  // Partial application
    val fieldTuples = this.getClass.getDeclaredFields map (x => (x, x.getName))
    fieldTuples map genFunc collect { case Some(x) => x } toSeq
  }

  protected def generateTestFunction(fieldData: (Field, String), regex: String) : Option[T]
  protected def construct(subject: U, testNumber: Int, shouldPass: Boolean, bundle: V) : T

}
