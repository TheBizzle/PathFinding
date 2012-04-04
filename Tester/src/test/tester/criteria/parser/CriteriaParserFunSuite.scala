package tester.criteria.parser

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import TestCriteriaParser._
import tester.criteria._
import util.matching.Regex

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/18/12
 * Time: 2:31 PM
 */

class CriteriaParserFunSuite extends FunSuite with ShouldMatchers {

  test("testingnessValue - Small positive integer") {
    testingnessValueShouldPass("1")
  }

  test("testingnessValue - Large positive integer") {
    testingnessValueShouldPass("32875971")
  }

  test("testingnessValue - ~<Small positive integer>") {
    testingnessValueShouldPass("~1")
  }

  test("testingnessValue - ~<Large positive integer>") {
    testingnessValueShouldPass("~32875971")
  }

  test("testingnessValue - Small positive integer with leading zero") {
    testingnessValueShouldFail("05")
  }

  test("testingnessValue - Zero") {
    testingnessValueShouldFail("0")
  }

  test("testingnessValue - Negative integer") {
    testingnessValueShouldFail("-1")
  }

  test("testingnessRange - Positive range") {
    testingnessRangeShouldPass("3->1599")
  }

  test("testingnessRange - Negated range") {
    testingnessRangeShouldPass("~13->15")
  }

  test("testingnessRange - Range including zero") {
    testingnessRangeShouldFail("0->5")
  }

  test("testingnessRange - Range including negative number") {
    testingnessRangeShouldFail("-1->3")
  }

  test("otherFlag - StackTrace") {
    otherFlagShouldPass("StackTrace", TestCriteriaToggleFlag(StackTrace))
  }

  test("otherFlag - RunBaseTests") {
    otherFlagShouldPass("RunBaseTests", TestCriteriaToggleFlag(RunBaseTests))
  }

  test("otherFlag - Talkative") {
    otherFlagShouldPass("Talkative", TestCriteriaToggleFlag(Talkative))
  }

  test("otherFlag - SkipExternalTests") {
    otherFlagShouldPass("SkipExternalTests", TestCriteriaToggleFlag(SkipExternalTests))
  }

  test("otherFlag - StackTrace (all lower)") {
    otherFlagShouldPass("stacktrace", TestCriteriaToggleFlag(StackTrace))
  }

  test("otherFlag - StackTrace (all upper)") {
    otherFlagShouldPass("STACKTRACE", TestCriteriaToggleFlag(StackTrace))
  }

  test("otherFlag - StackTrace (random mix of cases)") {
    otherFlagShouldPass("stACkTrACE", TestCriteriaToggleFlag(StackTrace))
  }

  test("otherFlag - Gibberish #1") {
    otherFlagShouldFail("AKJkjagnnannfa")
  }

  test("otherFlag - Gibberish #2") {
    otherFlagShouldFail("MajkLapder")
  }

  test("criteria - One value") {
    criteriaShouldPass("214")
  }

  test("criteria - Many values") {
    criteriaShouldPass("214", "~2", "332", "12", "443", "~67")
  }

  test("criteria - One range") {
    criteriaShouldPass("~214->340")
  }

  test("criteria - Many ranges") {
    criteriaShouldPass("~214->340", "1->4", "6->12", "~410->413")
  }

  test("criteria - One toggle") {
    criteriaShouldPass("RunBaseTests")
  }

  test("criteria - Many toggles") {
    criteriaShouldPass("RunBaseTests", "Talkative", "SkipExternalTests", "StackTrace")
  }

  test("criteria - Mixed #1") {
    criteriaShouldPass("RunBaseTests", "13", "24->50", "StackTrace", "~28", "~40->47", "2")
  }

  test("criteria - Mixed #2") {
    criteriaShouldPass("15", "17", "19", "~43", "30->300", "StackTrace", "SkipExternalTests", "~200->250", "~100->150", "~45->75")
  }

  test("criteria - Mixed #3") {
    criteriaShouldPass("30->60", "13", "~44", "Talkative")
  }
  
  test("criteria - Bad #1 (empty)") {
    criteriaShouldFail()
  }

  test("criteria - Bad #2 (bad flag)") {
    criteriaShouldFail("30->60", "13", "~44", "aaasdfaa")
  }

  test("criteria - Bad #3 (malformed data)") {
    criteriaShouldFail("30->60", "13||||4322||1", "~44", "Talkative")
  }

  // ======================== UTILITIES - TEST FUNCTIONS ============================

  private def testingnessValueShouldPass(arg: String) {
    val ValueTuple(not, value) = arg
    testShouldPass(testingnessValue, arg, TestRunningnessValue(value.toInt, not))
  }

  private def testingnessValueShouldFail(arg: String) {
    testShouldFail(testingnessValue, arg)
  }

  private def testingnessRangeShouldPass(arg: String) {
    val RangeTuple(not, start, end) = arg
    testShouldPass(testingnessRange, arg, TestRunningnessRange(start.toInt, end.toInt, not))
  }

  private def testingnessRangeShouldFail(arg: String) {
    testShouldFail(testingnessRange, arg)
  }

  private def otherFlagShouldPass(arg: String, target: TestCriteriaToggleFlag) {
    testShouldPass(otherFlag, arg, target)
  }

  private def otherFlagShouldFail(arg: String) {
    testShouldFail(otherFlag, arg)
  }

  private def criteriaShouldPass(args: String*) {
    testShouldPass(criteria, args.mkString(";"), args.toCriteria)
  }

  private def criteriaShouldFail(args: String*) {
    testShouldFail(criteria, args.mkString(";"))
  }

  private def testShouldPass[T](parser: TestCriteriaParser.Parser[T], testStr: String, target: T) {
    test(parser, testStr, ((x: ParseResult[_]) => x.get), target)
  }

  private def testShouldFail(parser: TestCriteriaParser.Parser[_], testStr: String) {
    test(parser, testStr, ((x: ParseResult[_]) => x.successful), false)
  }

  private def test[T](parser: TestCriteriaParser.Parser[_], testStr: String, resultMorpher: (ParseResult[_]) => T, target: T) {
    resultMorpher(TestCriteriaParser.parseAll(parser, testStr)) should  equal (target)
  }

  // ======================== UTILITIES - REGEX ============================

  // Courtesy of mkneissl on StackOverflow
  protected class RichRegex(underlying: Regex) {
    def matches(s: String) = underlying.pattern.matcher(s).matches
  }

  // Courtesy of mkneissl on StackOverflow
  implicit def regexToRichRegex(r: Regex) = new RichRegex(r)

  // ======================== UTILITIES - PARSING ============================

  implicit def not2Flag(not: String): TestRunningnessFlag = {
    if (not.isEmpty) RunTest else SkipTest
  }

  private val num = """[1-9][0-9]*"""
  private val ValueTuple = "(~?)(%s)".format(num).r
  private val RangeTuple = "(~?)(%s)->(%s)".format(Stream.continually(num) take 2: _*).r

  // ======================== UTILITIES - PIMPED SEQS ============================

  private class CriteriaSeq(strings: Seq[String]) {
    
    def toCriteria : Seq[TestCriteria] = strings map string2TestCriteria

    private def string2TestCriteria(s: String) : TestCriteria = {
      Some(s) collect { case x if (ValueTuple.matches(x)) => val ValueTuple(not, value) = s; TestRunningnessValue(value.toInt, not) } getOrElse (
        Some(s) collect { case x if (RangeTuple.matches(x)) =>
          val RangeTuple(not, start, end) = s; TestRunningnessRange(start.toInt, end.toInt, not) } getOrElse (
          Some(s) collect { case x if (x == "Talkative") => TestCriteriaToggleFlag(Talkative) } getOrElse (
            Some(s) collect { case x if (x == "StackTrace") => TestCriteriaToggleFlag(StackTrace) } getOrElse (
              Some(s) collect { case x if (x == "SkipExternalTests") => TestCriteriaToggleFlag(SkipExternalTests) } getOrElse (
                Some(s) collect { case x if (x == "RunBaseTests") => TestCriteriaToggleFlag(RunBaseTests) } get ))))).asInstanceOf[TestCriteria]
    }
    
  }

  implicit private def stringSeq2TestCriteriaSeq(strings: Seq[String]) : CriteriaSeq = {
    new CriteriaSeq(strings)
  }

}
