package tester

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import collection.immutable.HashMap
import tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 7:22 PM
 */

class TesterFunSuite extends FunSuite with ShouldMatchers {

  test("handleTestIntervals - Empty, one") {
    val inValues  = Seq()
    val inRanges  = Seq(TestRunningnessRange(1, 2, RunTest))
    val result    = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expecteds = Seq(1, 2)
    result should equal (expecteds)
  }

  test("handleTestIntervals - One, empty") {
    val inValues  = Seq(TestRunningnessValue(2, RunTest))
    val inRanges  = Seq()
    val result    = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expecteds = Seq(2)
    result should equal (expecteds)
  }

  test("handleTestIntervals - One, one") {
    val inValues  = Seq(TestRunningnessValue(2, RunTest))
    val inRanges  = Seq(TestRunningnessRange(4, 6, RunTest))
    val result    = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expecteds = Seq(2, 4, 5, 6)
    result should equal (expecteds)
  }

  test("handleTestIntervals - Many, many") {
    val inValues = Seq(TestRunningnessValue(5, RunTest), TestRunningnessValue(1, SkipTest))
    val inRanges = Seq(TestRunningnessRange(1, 4, RunTest), TestRunningnessRange(2, 3, SkipTest))
    val result = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expecteds = Seq(4, 5)
    result should equal (expecteds)
  }

  test("assessPathingDesire - Empty map") {
    val inMap = HashMap[String, Seq[TestCriteria]](TestingCore.ArgKeyValue  -> Seq[TestRunningnessValue](),
    TestingCore.ArgKeyRange  -> Seq[TestRunningnessRange](),
    TestingCore.ArgKeyToggle -> Seq[TestCriteriaToggleFlag]())
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (false)
  }

  test("assessPathingDesire - Map without values/ranges") {
    val inMap = HashMap[String, Seq[TestCriteria]](TestingCore.ArgKeyValue  -> Seq[TestRunningnessValue](),
    TestingCore.ArgKeyRange  -> Seq[TestRunningnessRange](),
    TestingCore.ArgKeyToggle -> Seq[TestCriteriaToggleFlag](SkipExternalTests))
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (false)
  }

  test("assessPathingDesire - Map without ranges") {
    val inMap = HashMap[String, Seq[TestCriteria]](TestingCore.ArgKeyValue  -> Seq[TestRunningnessValue](TestRunningnessValue(1, RunTest)),
    TestingCore.ArgKeyRange  -> Seq[TestRunningnessRange](),
    TestingCore.ArgKeyToggle -> Seq[TestCriteriaToggleFlag](SkipExternalTests))
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (true)
  }

  test("assessPathingDesire - Map without values") {
    val inMap = HashMap[String, Seq[TestCriteria]](TestingCore.ArgKeyValue  -> Seq[TestRunningnessValue](),
    TestingCore.ArgKeyRange  -> Seq[TestRunningnessRange](TestRunningnessRange(1, 1, RunTest)),
    TestingCore.ArgKeyToggle -> Seq[TestCriteriaToggleFlag](SkipExternalTests))
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (true)
  }

  test("assessPathingDesire - Mixed map") {
    val inMap = HashMap[String, Seq[TestCriteria]](TestingCore.ArgKeyValue  -> Seq[TestRunningnessValue](TestRunningnessValue(3, RunTest)),
    TestingCore.ArgKeyRange  -> Seq[TestRunningnessRange](TestRunningnessRange(1, 1, RunTest), TestRunningnessRange(2, 2, RunTest)),
    TestingCore.ArgKeyToggle -> Seq[TestCriteriaToggleFlag]())
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (true)
  }

  test("sortCriteria - Empty") {
    val crits = Seq()
    val results = TestingCore.sortCriteria(crits)
    val expected = Seq()
    results should equal (expected)
  }

  test("sortCriteria - One value") {
    val crits = Seq(TestRunningnessValue(1, RunTest))
    val results = TestingCore.sortCriteria(crits)
    val expected = crits
    results should equal (expected)
  }

  test("sortCriteria - One range") {
    val crits = Seq(TestRunningnessRange(1, 1, RunTest))
    val results = TestingCore.sortCriteria(crits)
    val expected = crits
    results should equal (expected)
  }

  test("sortCriteria - Many values (presorted)") {
    val crits = Seq(TestRunningnessValue(1, RunTest), TestRunningnessValue(3, RunTest), TestRunningnessValue(17, RunTest))
    val results = TestingCore.sortCriteria(crits)
    val expected = crits
    results should equal (expected)
  }

  test("sortCriteria - Many ranges (presorted)") {
    val crits = Seq(TestRunningnessRange(1, 6, RunTest), TestRunningnessRange(7, 7, RunTest),
    TestRunningnessRange(8, 9, RunTest), TestRunningnessRange(12, 17, RunTest))
    val results = TestingCore.sortCriteria(crits)
    val expected = crits
    results should equal (expected)
  }

  test("sortCriteria - Many values (unsorted)") {
    val crits = Seq(TestRunningnessValue(17, RunTest), TestRunningnessValue(1, RunTest), TestRunningnessValue(3, RunTest))
    val results = TestingCore.sortCriteria(crits)
    val expected = Seq(TestRunningnessValue(1, RunTest), TestRunningnessValue(3, RunTest), TestRunningnessValue(17, RunTest))
    results should equal (expected)
  }

  test("sortCriteria - Many ranges (reversed)") {
    val crits = Seq(TestRunningnessRange(12, 17, RunTest), TestRunningnessRange(8, 9, RunTest),
    TestRunningnessRange(7, 7, RunTest), TestRunningnessRange(1, 6, RunTest))
    val results = TestingCore.sortCriteria(crits)
    val expected = crits.reverse
    results should equal (expected)
  }

  test("handleRanges - Empty") {
    val crits = Seq()
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(crits, 6)
    val expectedTests = Seq()
    val expectedSkips = Seq()
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - One run") {
    val crits = Seq(TestRunningnessRange(1, 3, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(crits, 6)
    val expectedTests = crits
    val expectedSkips = Seq()
    val expectedMax = 3
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - One skip") {
    val crits = Seq(TestRunningnessRange(1, 3, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(crits, 6)
    val expectedTests = Seq()
    val expectedSkips = crits
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - Many runs") {
    val crits = Seq(TestRunningnessRange(1, 3, RunTest), TestRunningnessRange(4, 4, RunTest), TestRunningnessRange(5, 6, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(crits, 6)
    val expectedTests = crits
    val expectedSkips = Seq()
    val expectedMax = 6
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - Many skips") {
    val crits = Seq(TestRunningnessRange(1, 3, SkipTest), TestRunningnessRange(4, 4, SkipTest), TestRunningnessRange(5, 6, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(crits, 6)
    val expectedTests = Seq()
    val expectedSkips = crits
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - Many (mixed)") {
    val crits = Seq(TestRunningnessRange(1, 3, SkipTest), TestRunningnessRange(4, 4, RunTest), TestRunningnessRange(5, 6, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(crits, 6)
    val expectedTests = Seq(TestRunningnessRange(4, 4, RunTest))
    val expectedSkips = Seq(TestRunningnessRange(1, 3, SkipTest), TestRunningnessRange(5, 6, SkipTest))
    val expectedMax = 4
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Empty") {
    val crits = Seq()
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(crits, 6)
    val expectedTests = Seq()
    val expectedSkips = Seq()
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - One run") {
    val crits = Seq(TestRunningnessValue(1, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(crits, 6)
    val expectedTests = crits
    val expectedSkips = Seq()
    val expectedMax = 1
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - One skip") {
    val crits = Seq(TestRunningnessValue(1, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(crits, 6)
    val expectedTests = Seq()
    val expectedSkips = crits
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Many runs") {
    val crits = Seq(TestRunningnessValue(1, RunTest), TestRunningnessValue(4, RunTest), TestRunningnessValue(5, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(crits, 6)
    val expectedTests = crits
    val expectedSkips = Seq()
    val expectedMax = 5
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Many skips") {
    val crits = Seq(TestRunningnessValue(1, SkipTest), TestRunningnessValue(4, SkipTest),  TestRunningnessValue(5, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(crits, 6)
    val expectedTests = Seq()
    val expectedSkips = crits
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Many (mixed)") {
    val crits = Seq(TestRunningnessValue(1, SkipTest), TestRunningnessValue(4, RunTest), TestRunningnessValue(5, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(crits, 6)
    val expectedTests = Seq(TestRunningnessValue(4, RunTest))
    val expectedSkips = Seq(TestRunningnessValue(1, SkipTest), TestRunningnessValue(5, SkipTest))
    val expectedMax = 4
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("containsOverlaps - Empty") {

    val crits = Seq()
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(crits)

    // The "should" notation is breaking again...
    result === false
    offenderPart1 === None
    offenderPart2 === None

  }

  test("containsOverlaps - One") {

    val crits = Seq(TestRunningnessRange(1, 1, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(crits)

    result === false
    offenderPart1 === None
    offenderPart2 === None

  }

  test("containsOverlaps - Many (without overlap)") {

    val crits = Seq(TestRunningnessRange(1, 1, RunTest), TestRunningnessRange(3, 7, RunTest), TestRunningnessRange(9, 9, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(crits)

    result === false
    offenderPart1 === None
    offenderPart2 === None

  }

  test("containsOverlaps - Many (with SOME overlap)") {

    val crits = Seq(TestRunningnessRange(1, 1, RunTest), TestRunningnessRange(3, 7, RunTest), TestRunningnessRange(7, 9, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(crits)

    result === true
    offenderPart1 === Option(TestRunningnessRange(3, 7, RunTest))
    offenderPart2 === Option(TestRunningnessRange(7, 9, RunTest))

  }

  test("containsOverlaps - Many (with ALL overlaps)") {

    val crits = Seq(TestRunningnessRange(1, 5, RunTest), TestRunningnessRange(3, 7, RunTest), TestRunningnessRange(7, 9, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(crits)

    result === true
    offenderPart1 === Option(TestRunningnessRange(1, 5, RunTest))
    offenderPart2 === Option(TestRunningnessRange(3, 7, RunTest))

  }

  test("generateResultArray - Empty, empty, empty, empty, 10") {
    val resultArr = TestingCore.generateResultArray(Seq(), Seq(), Seq(), Seq(), 10)
    val expected = Array(false, false, false, false, false,
    false, false, false, false, false, false)
    resultArr should equal (expected)
  }

  test("generateResultArray - One, one, one, one, 10") {
    val resultArr = TestingCore.generateResultArray(Seq(TestRunningnessRange(2, 8, RunTest)), Seq(TestRunningnessValue(10, RunTest)),
    Seq(TestRunningnessRange(4, 5, SkipTest)), Seq(TestRunningnessValue(7, SkipTest)), 10)
    val expected = Array(false, false, true, true, false,
    false, true, false, true, false, true)
    resultArr should equal (expected)
  }

  test("sortArgLists - Empty list") {
    val resultMap = TestingCore.sortArgLists(Seq())
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Seq())
    resultRanges should equal (Seq())
    resultToggles should equal (Seq())
  }

  test("sortArgLists - One value") {
    val inValue = TestRunningnessValue(1, RunTest)
    val resultMap = TestingCore.sortArgLists(Seq(inValue))
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Seq(inValue))
    resultRanges should equal (Seq())
    resultToggles should equal (Seq())
  }

  test("sortArgLists - One range") {
    val inRange = TestRunningnessRange(1, 1, RunTest)
    val resultMap = TestingCore.sortArgLists(Seq(inRange))
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Seq())
    resultRanges should equal (Seq(inRange))
    resultToggles should equal (Seq())
  }

  test("sortArgLists - One toggle") {
    val inToggle = TestCriteriaToggleFlag(SkipExternalTests)
    val resultMap = TestingCore.sortArgLists(Seq(inToggle))
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Seq())
    resultRanges should equal (Seq())
    resultToggles should equal (Seq(inToggle))
  }

  test("sortArgLists - Many mixed") {

    val inValue1 = TestRunningnessValue(11, SkipTest)
    val inValue2 = TestRunningnessValue(15, RunTest)

    val inRange1 = TestRunningnessRange(1, 6, RunTest)
    val inRange2 = TestRunningnessRange(2, 4, SkipTest)
    val inRange3 = TestRunningnessRange(10, 13, RunTest)

    val inToggle1 = TestCriteriaToggleFlag(SkipExternalTests)
    val inToggle2 = TestCriteriaToggleFlag(SkipExternalTests)

    val resultMap = TestingCore.sortArgLists(Seq(inValue1, inToggle1, inRange3, inRange1, inToggle2, inRange2, inValue2))
    val (values, ranges, toggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))

    // Why isn't the "should" syntax working here...?
    values.contains(inValue1) === true
    values.contains(inValue2) === true

    ranges.contains(inRange1) === true
    ranges.contains(inRange2) === true
    ranges.contains(inRange3) === true

    toggles.contains(inToggle1) === true
    toggles.contains(inToggle2) === true

  }

}
