package tester

import
  org.scalatest.{ FunSuite, matchers },
    matchers.ShouldMatchers

import
  criteria._,
  TestCriteriaDialect._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/15/12
 * Time: 3:30 PM
 */

// There are no tests for failure, because those are caught at compile time!
class CriteriaDialectFunSuite extends FunSuite with ShouldMatchers {

  test("Value - Integer") {
    val num = 1
    val flag = RunTest
    testValue(num, (num, flag))
  }

  test("Value - !Integer") {
    val num = 1
    val flag = SkipTest
    testValue(!num, (num, flag))
  }

  test("Range") {
    val start = 3
    val end = 1599
    val flag = RunTest
    testRange(start >&> end, (start, end, flag))
  }

  test("Range - Not-range") {
    val start = 13
    val end = 15
    val flag = SkipTest
    testRange(start >!> end, (start, end, flag))
  }

  test("Flag - StackTrace") {
    testFlag(StackTrace, TestCriteriaToggleFlag(StackTrace))
  }

  test("Flag - RunBaseTests") {
    testFlag(RunBaseTests, TestCriteriaToggleFlag(RunBaseTests))
  }

  test("Flag - Talkative") {
    testFlag(Talkative, TestCriteriaToggleFlag(Talkative))
  }

  test("Flag - SkipExternalTests") {
    testFlag(SkipExternalTests, TestCriteriaToggleFlag(SkipExternalTests))
  }

  test("Criteria - One value") {
    val crit = (Some(214), None, RunTest)
    testCriteria(crit._1.get ^^, crit)
  }

  test("Criteria - Many values") {
    val crit1 = (Some(214), None, RunTest)
    val crit2 = (Some(2),   None, SkipTest)
    val crit3 = (Some(332), None, RunTest)
    val crit4 = (Some(12),  None, RunTest)
    val crit5 = (Some(443), None, RunTest)
    val crit6 = (Some(67),  None, SkipTest)
    testCriteria(crit1._1.get &&
                !crit2._1.get &&
                 crit3._1.get &&
                 crit4._1.get &&
                 crit5._1.get &&
                !crit6._1.get,
                 crit1, crit2, crit3, crit4, crit5, crit6)
  }

  test("Criteria - One range") {
    val crit = (Some(214), Some(340), SkipTest)
    testCriteria(crit._1.get >!> crit._2.get, crit)
  }

  test("Criteria - Many ranges") {
    val crit1 = (Some(214), Some(340), SkipTest)
    val crit2 = (Some(1),   Some(4),   RunTest)
    val crit3 = (Some(6),   Some(12),  RunTest)
    val crit4 = (Some(410), Some(413), SkipTest)
    testCriteria(crit1._1.get >!> crit1._2.get &&
                 crit2._1.get >&> crit2._2.get &&
                 crit3._1.get >&> crit3._2.get &&
                 crit4._1.get >!> crit4._2.get,
                 crit1, crit2, crit3, crit4)
  }

  test("Criteria - One toggle") {
    val crit = (None, None, RunBaseTests)
    testCriteria(crit._3 ^^, crit)
  }

  test("Criteria - Many toggles") {
    val crit1 = (None, None, RunBaseTests)
    val crit2 = (None, None, Talkative)
    val crit3 = (None, None, SkipExternalTests)
    val crit4 = (None, None, StackTrace)
    testCriteria(crit1._3 && crit2._3 && crit3._3 && crit4._3, crit1, crit2, crit3, crit4)
  }

  test("Criteria - Mixed #1") {
    val crit1 = (None,     None,     RunBaseTests)
    val crit2 = (Some(13), None,     RunTest)
    val crit3 = (Some(24), Some(50), RunTest)
    val crit4 = (None,     None,     StackTrace)
    val crit5 = (Some(28), None,     SkipTest)
    val crit6 = (Some(40), Some(47), SkipTest)
    val crit7 = (Some(2),  None,     RunTest)
    testCriteria(crit1._3 &&
                 crit2._1.get &&
                 crit3._1.get >&> crit3._2.get &&
                 crit4._3 &&
                !crit5._1.get &&
                 crit6._1.get >!> crit6._2.get &&
                 crit7._1.get,
                 crit1, crit2, crit3, crit4, crit5, crit6, crit7)
  }

  test("Criteria - Mixed #2") {
    val crit1  = (Some(15),  None,      RunTest)
    val crit2  = (Some(17),  None,      RunTest)
    val crit3  = (Some(19),  None,      RunTest)
    val crit4  = (Some(43),  None,      SkipTest)
    val crit5  = (Some(30),  Some(300), RunTest)
    val crit6  = (None,      None,      StackTrace)
    val crit7  = (None,      None,      SkipExternalTests)
    val crit8  = (Some(200), Some(250), SkipTest)
    val crit9  = (Some(100), Some(150), SkipTest)
    val crit10 = (Some(45),  Some(75),  SkipTest)
    testCriteria(crit1._1.get &&
                 crit2._1.get &&
                 crit3._1.get &&
                !crit4._1.get &&
                 crit5._1.get >&> crit5._2.get &&
                 crit6._3 &&
                 crit7._3 &&
                 crit8._1.get >!> crit8._2.get &&
                 crit9._1.get >!> crit9._2.get &&
                 crit10._1.get >!> crit10._2.get,
                 crit1, crit2, crit3, crit4, crit5, crit6, crit7, crit8, crit9, crit10)
  }

  test("Criteria - Mixed #3") {
    val crit1 = (Some(30), Some(60), RunTest)
    val crit2 = (Some(13), None,     RunTest)
    val crit3 = (Some(44), None,     SkipTest)
    val crit4 = (None,     None,     Talkative)
    testCriteria(crit1._1.get >&> crit1._2.get &&
                 crit2._1.get &&
                !crit3._1.get &&
                 crit4._3,
                 crit1, crit2, crit3, crit4)
  }

  test("Criteria - Mixed #4") {
    val crit1 = (Some(1),  Some(10), RunTest)
    val crit2 = (None,     None,     StackTrace)
    val crit3 = (Some(12), None,     RunTest)
    val crit4 = (Some(14), None,     RunTest)
    val crit5 = (None,     None,     Talkative)
    val crit6 = (Some(18), Some(30), RunTest)
    val crit7 = (Some(24), Some(26), SkipTest)
    val crit8 = (Some(19), None,     SkipTest)
    testCriteria(crit1._1.get >&> crit1._2.get &&
                 crit2._3 &&
                 crit3._1.get &&
                 crit4._1.get &&
                 crit5._3 &&
                 crit6._1.get >&> crit6._2.get &&
                 crit7._1.get >!> crit7._2.get &&
                !crit8._1.get,
                 crit1, crit2, crit3, crit4, crit5, crit6, crit7, crit8)
  }

  test("Criteria - Empty") {
    testCriteria(Seq())
  }

  // ======================== UTILITIES - TEST FUNCTIONS ============================

  private def testValue[T <% PimpedValueTuple](subject: T, numAndFlag: (Int, TestRunningnessFlag)) {
    test(subject.valueTuple, TestRunningnessValue(numAndFlag._1, numAndFlag._2))
  }

  private def testRange(subject: PimpedRangeTuple, numsAndFlag: (Int, Int, TestRunningnessFlag)) {
    test(subject.rangeTuple, TestRunningnessRange(numsAndFlag._1, numsAndFlag._2, numsAndFlag._3))
  }
  
  private def testFlag[T <% TestCriteriaToggleFlag](subject: T, flag: TestCriteriaToggleFlag) {
    test(subject, flag)
  }

  private def testCriteria(subject: Seq[TestCriteria], target: (Option[Int], Option[Int], TestingFlag)*) {
    require(subject.size == target.size)
    subject zip target foreach {
      case (s: TestCriteriaToggleFlag, (None, None, flag: TestToggleFlag))                  => testFlag(s, TestCriteriaToggleFlag(flag))
      case (s: TestRunningnessValue,   (Some(num), None, flag: TestRunningnessFlag))        => testValue(PimpedValueTuple(s), (num, flag))
      case (s: TestRunningnessRange,   (Some(start), Some(end), flag: TestRunningnessFlag)) => testRange(PimpedRangeTuple(s), (start, end, flag))
      case (s, t)                                                                           => throw new MatchError("Failed to match %s with %s".format(s, t))
      case  x                                                                               => throw new MatchError("Failed to match %s".format(x))
    }
  }

  private def test(subject: TestCriteria, target: TestCriteria) {
    subject should equal (target)
  }

}
