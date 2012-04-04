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
    val inValues = Nil
    val inRanges = List(TestRunningnessRange(1, 2, RunTest))
    val result = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expectedList = List(1, 2)
    result should equal (expectedList)
  }

  test("handleTestIntervals - One, empty") {
    val inValues = List(TestRunningnessValue(2, RunTest))
    val inRanges = Nil
    val result = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expectedList = List(2)
    result should equal (expectedList)
  }

  test("handleTestIntervals - One, one") {
    val inValues = List(TestRunningnessValue(2, RunTest))
    val inRanges = List(TestRunningnessRange(4, 6, RunTest))
    val result = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expectedList = List(2, 4, 5, 6)
    result should equal (expectedList)
  }

  test("handleTestIntervals - Many, many") {
    val inValues = List(TestRunningnessValue(5, RunTest), TestRunningnessValue(1, SkipTest))
    val inRanges = List(TestRunningnessRange(1, 4, RunTest), TestRunningnessRange(2, 3, SkipTest))
    val result = TestingCore.handleTestIntervals(inValues, inRanges, 6)
    val expectedList = List(4, 5)
    result should equal (expectedList)
  }

  test("assessPathingDesire - Empty map") {
    val inMap = HashMap[String, List[TestCriteria]](TestingCore.ArgKeyValue  -> List[TestRunningnessValue](),
    TestingCore.ArgKeyRange  -> List[TestRunningnessRange](),
    TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag]())
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (false)
  }

  test("assessPathingDesire - Map without values/ranges") {
    val inMap = HashMap[String, List[TestCriteria]](TestingCore.ArgKeyValue  -> List[TestRunningnessValue](),
    TestingCore.ArgKeyRange  -> List[TestRunningnessRange](),
    TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag](SkipExternalTests))
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (false)
  }

  test("assessPathingDesire - Map without ranges") {
    val inMap = HashMap[String, List[TestCriteria]](TestingCore.ArgKeyValue  -> List[TestRunningnessValue](TestRunningnessValue(1, RunTest)),
    TestingCore.ArgKeyRange  -> List[TestRunningnessRange](),
    TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag](SkipExternalTests))
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (true)
  }

  test("assessPathingDesire - Map without values") {
    val inMap = HashMap[String, List[TestCriteria]](TestingCore.ArgKeyValue  -> List[TestRunningnessValue](),
    TestingCore.ArgKeyRange  -> List[TestRunningnessRange](TestRunningnessRange(1, 1, RunTest)),
    TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag](SkipExternalTests))
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (true)
  }

  test("assessPathingDesire - Mixed map") {
    val inMap = HashMap[String, List[TestCriteria]](TestingCore.ArgKeyValue  -> List[TestRunningnessValue](TestRunningnessValue(3, RunTest)),
    TestingCore.ArgKeyRange  -> List[TestRunningnessRange](TestRunningnessRange(1, 1, RunTest), TestRunningnessRange(2, 2, RunTest)),
    TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag]())
    val result = TestingCore.assessExternalityDesire(inMap)
    result should equal (true)
  }

  test("sortCriteria - Empty") {
    val inList = Nil
    val resultList = TestingCore.sortCriteria(inList)
    val expected = Nil
    resultList should equal (expected)
  }

  test("sortCriteria - One value") {
    val inList = List(TestRunningnessValue(1, RunTest))
    val resultList = TestingCore.sortCriteria(inList)
    val expected = inList
    resultList should equal (expected)
  }

  test("sortCriteria - One range") {
    val inList = List(TestRunningnessRange(1, 1, RunTest))
    val resultList = TestingCore.sortCriteria(inList)
    val expected = inList
    resultList should equal (expected)
  }

  test("sortCriteria - Many values (presorted)") {
    val inList = List(TestRunningnessValue(1, RunTest), TestRunningnessValue(3, RunTest), TestRunningnessValue(17, RunTest))
    val resultList = TestingCore.sortCriteria(inList)
    val expected = inList
    resultList should equal (expected)
  }

  test("sortCriteria - Many ranges (presorted)") {
    val inList = List(TestRunningnessRange(1, 6, RunTest), TestRunningnessRange(7, 7, RunTest),
    TestRunningnessRange(8, 9, RunTest), TestRunningnessRange(12, 17, RunTest))
    val resultList = TestingCore.sortCriteria(inList)
    val expected = inList
    resultList should equal (expected)
  }

  test("sortCriteria - Many values (unsorted)") {
    val inList = List(TestRunningnessValue(17, RunTest), TestRunningnessValue(1, RunTest), TestRunningnessValue(3, RunTest))
    val resultList = TestingCore.sortCriteria(inList)
    val expected = List(TestRunningnessValue(1, RunTest), TestRunningnessValue(3, RunTest), TestRunningnessValue(17, RunTest))
    resultList should equal (expected)
  }

  test("sortCriteria - Many ranges (reversed)") {
    val inList = List(TestRunningnessRange(12, 17, RunTest), TestRunningnessRange(8, 9, RunTest),
    TestRunningnessRange(7, 7, RunTest), TestRunningnessRange(1, 6, RunTest))
    val resultList = TestingCore.sortCriteria(inList)
    val expected = inList.reverse
    resultList should equal (expected)
  }

  test("handleRanges - Empty") {
    val inList = Nil
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList, 6)
    val expectedTests = Nil
    val expectedSkips = Nil
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - One run") {
    val inList = List(TestRunningnessRange(1, 3, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList, 6)
    val expectedTests = inList
    val expectedSkips = Nil
    val expectedMax = 3
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - One skip") {
    val inList = List(TestRunningnessRange(1, 3, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList, 6)
    val expectedTests = Nil
    val expectedSkips = inList
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - Many runs") {
    val inList = List(TestRunningnessRange(1, 3, RunTest), TestRunningnessRange(4, 4, RunTest), TestRunningnessRange(5, 6, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList, 6)
    val expectedTests = inList
    val expectedSkips = Nil
    val expectedMax = 6
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - Many skips") {
    val inList = List(TestRunningnessRange(1, 3, SkipTest), TestRunningnessRange(4, 4, SkipTest), TestRunningnessRange(5, 6, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList, 6)
    val expectedTests = Nil
    val expectedSkips = inList
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleRanges - Many (mixed)") {
    val inList = List(TestRunningnessRange(1, 3, SkipTest), TestRunningnessRange(4, 4, RunTest), TestRunningnessRange(5, 6, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList, 6)
    val expectedTests = List(TestRunningnessRange(4, 4, RunTest))
    val expectedSkips = List(TestRunningnessRange(1, 3, SkipTest), TestRunningnessRange(5, 6, SkipTest))
    val expectedMax = 4
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Empty") {
    val inList = Nil
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList, 6)
    val expectedTests = Nil
    val expectedSkips = Nil
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - One run") {
    val inList = List(TestRunningnessValue(1, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList, 6)
    val expectedTests = inList
    val expectedSkips = Nil
    val expectedMax = 1
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - One skip") {
    val inList = List(TestRunningnessValue(1, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList, 6)
    val expectedTests = Nil
    val expectedSkips = inList
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Many runs") {
    val inList = List(TestRunningnessValue(1, RunTest), TestRunningnessValue(4, RunTest), TestRunningnessValue(5, RunTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList, 6)
    val expectedTests = inList
    val expectedSkips = Nil
    val expectedMax = 5
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Many skips") {
    val inList = List(TestRunningnessValue(1, SkipTest), TestRunningnessValue(4, SkipTest),  TestRunningnessValue(5, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList, 6)
    val expectedTests = Nil
    val expectedSkips = inList
    val expectedMax = 0
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("handleValues - Many (mixed)") {
    val inList = List(TestRunningnessValue(1, SkipTest), TestRunningnessValue(4, RunTest), TestRunningnessValue(5, SkipTest))
    val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList, 6)
    val expectedTests = List(TestRunningnessValue(4, RunTest))
    val expectedSkips = List(TestRunningnessValue(1, SkipTest), TestRunningnessValue(5, SkipTest))
    val expectedMax = 4
    resultTests should equal (expectedTests)
    resultSkips should equal (expectedSkips)
    resultMax   should equal (expectedMax)
  }

  test("containsOverlaps - Empty") {

    val inList = Nil
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

    // The "should" notation is breaking again...
    result === false
    offenderPart1 === None
    offenderPart2 === None

  }

  test("containsOverlaps - One") {

    val inList = List(TestRunningnessRange(1, 1, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

    result === false
    offenderPart1 === None
    offenderPart2 === None

  }

  test("containsOverlaps - Many (without overlap)") {

    val inList = List(TestRunningnessRange(1, 1, RunTest), TestRunningnessRange(3, 7, RunTest), TestRunningnessRange(9, 9, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

    result === false
    offenderPart1 === None
    offenderPart2 === None

  }

  test("containsOverlaps - Many (with SOME overlap)") {

    val inList = List(TestRunningnessRange(1, 1, RunTest), TestRunningnessRange(3, 7, RunTest), TestRunningnessRange(7, 9, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

    result === true
    offenderPart1 === Option(TestRunningnessRange(3, 7, RunTest))
    offenderPart2 === Option(TestRunningnessRange(7, 9, RunTest))

  }

  test("containsOverlaps - Many (with ALL overlaps)") {

    val inList = List(TestRunningnessRange(1, 5, RunTest), TestRunningnessRange(3, 7, RunTest), TestRunningnessRange(7, 9, RunTest))
    val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

    result === true
    offenderPart1 === Option(TestRunningnessRange(1, 5, RunTest))
    offenderPart2 === Option(TestRunningnessRange(3, 7, RunTest))

  }

  test("generateResultArray - Empty, empty, empty, empty, 10") {
    val resultArr = TestingCore.generateResultArray(Nil, Nil, Nil, Nil, 10)
    val expected = Array(false, false, false, false, false,
    false, false, false, false, false, false)
    resultArr should equal (expected)
  }

  test("generateResultArray - One, one, one, one, 10") {
    val resultArr = TestingCore.generateResultArray(List(TestRunningnessRange(2, 8, RunTest)), List(TestRunningnessValue(10, RunTest)),
    List(TestRunningnessRange(4, 5, SkipTest)), List(TestRunningnessValue(7, SkipTest)), 10)
    val expected = Array(false, false, true, true, false,
    false, true, false, true, false, true)
    resultArr should equal (expected)
  }

  test("sortArgLists - Empty list") {
    val resultMap = TestingCore.sortArgLists(Nil)
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Nil)
    resultRanges should equal (Nil)
    resultToggles should equal (Nil)
  }

  test("sortArgLists - One value") {
    val inValue = TestRunningnessValue(1, RunTest)
    val resultMap = TestingCore.sortArgLists(List(inValue))
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (List(inValue))
    resultRanges should equal (Nil)
    resultToggles should equal (Nil)
  }

  test("sortArgLists - One range") {
    val inRange = TestRunningnessRange(1, 1, RunTest)
    val resultMap = TestingCore.sortArgLists(List(inRange))
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Nil)
    resultRanges should equal (List(inRange))
    resultToggles should equal (Nil)
  }

  test("sortArgLists - One toggle") {
    val inToggle = TestCriteriaToggleFlag(SkipExternalTests)
    val resultMap = TestingCore.sortArgLists(List(inToggle))
    val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
    resultValues should equal (Nil)
    resultRanges should equal (Nil)
    resultToggles should equal (List(inToggle))
  }

  test("sortArgLists - Many mixed") {

    val inValue1 = TestRunningnessValue(11, SkipTest)
    val inValue2 = TestRunningnessValue(15, RunTest)

    val inRange1 = TestRunningnessRange(1, 6, RunTest)
    val inRange2 = TestRunningnessRange(2, 4, SkipTest)
    val inRange3 = TestRunningnessRange(10, 13, RunTest)

    val inToggle1 = TestCriteriaToggleFlag(SkipExternalTests)
    val inToggle2 = TestCriteriaToggleFlag(SkipExternalTests)

    val resultMap = TestingCore.sortArgLists(List(inValue1, inToggle1, inRange3, inRange1, inToggle2, inRange2, inValue2))
    val (valueList, rangeList, toggleList) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))

    // Why isn't the "should" syntax working here...?
    valueList.contains(inValue1) === true
    valueList.contains(inValue2) === true

    rangeList.contains(inRange1) === true
    rangeList.contains(inRange2) === true
    rangeList.contains(inRange3) === true

    toggleList.contains(inToggle1) === true
    toggleList.contains(inToggle2) === true

  }

}
