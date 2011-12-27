package pathfinding.tester.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import pathfinding.tester.TestingCore
import pathfinding.tester.criteria._
import collection.immutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 7:22 PM
 */

class TesterFunSuite extends FunSuite with ShouldMatchers {

    test("handleTestIntervals - Empty, one") {
        val inValues = Nil
        val inRanges = List(TestCriteriaRangeTuple(1, 2, RunRange))
        val result = TestingCore.handleTestIntervals(inValues, inRanges)
        val expectedList = List(1, 2)
        result should equal (expectedList)
    }

    test("handleTestIntervals - One, empty") {
        val inValues = List(TestCriteriaValueTuple(2, RunTest))
        val inRanges = Nil
        val result = TestingCore.handleTestIntervals(inValues, inRanges)
        val expectedList = List(2)
        result should equal (expectedList)
    }

    test("handleTestIntervals - One, one") {
        val inValues = List(TestCriteriaValueTuple(2, RunTest))
        val inRanges = List(TestCriteriaRangeTuple(4, 6, RunRange))
        val result = TestingCore.handleTestIntervals(inValues, inRanges)
        val expectedList = List(2, 4, 5, 6)
        result should equal (expectedList)
    }

    test("handleTestIntervals - Many, many") {
        val inValues = List(TestCriteriaValueTuple(5, RunTest), TestCriteriaValueTuple(1, SkipTest))
        val inRanges = List(TestCriteriaRangeTuple(1, 4, RunRange), TestCriteriaRangeTuple(2, 3, SkipRange))
        val result = TestingCore.handleTestIntervals(inValues, inRanges)
        val expectedList = List(4, 5)
        result should equal (expectedList)
    }

    test("assessPathingDesire - Empty map") {
        val inMap = HashMap[String, List[TestCriteria[_]]](TestingCore.ArgKeyValue  -> List[TestCriteriaValueTuple](),
                                                           TestingCore.ArgKeyRange  -> List[TestCriteriaRangeTuple](),
                                                           TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag]())
        val result = TestingCore.assessPathingDesire(inMap)
        result should equal (false)
    }

    test("assessPathingDesire - Map without values/ranges") {
        val inMap = HashMap[String, List[TestCriteria[_]]](TestingCore.ArgKeyValue  -> List[TestCriteriaValueTuple](),
                                                           TestingCore.ArgKeyRange  -> List[TestCriteriaRangeTuple](),
                                                           TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag](SkipPathingTests))
        val result = TestingCore.assessPathingDesire(inMap)
        result should equal (false)
    }

    test("assessPathingDesire - Map without ranges") {
        val inMap = HashMap[String, List[TestCriteria[_]]](TestingCore.ArgKeyValue  -> List[TestCriteriaValueTuple](TestCriteriaValueTuple(1, RunTest)),
                                                           TestingCore.ArgKeyRange  -> List[TestCriteriaRangeTuple](),
                                                           TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag](SkipPathingTests))
        val result = TestingCore.assessPathingDesire(inMap)
        result should equal (true)
    }

    test("assessPathingDesire - Map without values") {
        val inMap = HashMap[String, List[TestCriteria[_]]](TestingCore.ArgKeyValue  -> List[TestCriteriaValueTuple](),
                                                           TestingCore.ArgKeyRange  -> List[TestCriteriaRangeTuple](TestCriteriaRangeTuple(1, 1, RunRange)),
                                                           TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag](SkipPathingTests))
        val result = TestingCore.assessPathingDesire(inMap)
        result should equal (true)
    }

    test("assessPathingDesire - Mixed map") {
        val inMap = HashMap[String, List[TestCriteria[_]]](TestingCore.ArgKeyValue  -> List[TestCriteriaValueTuple](TestCriteriaValueTuple(3, RunTest)),
                                                           TestingCore.ArgKeyRange  -> List[TestCriteriaRangeTuple](TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(2, 2, RunRange)),
                                                           TestingCore.ArgKeyToggle -> List[TestCriteriaToggleFlag]())
        val result = TestingCore.assessPathingDesire(inMap)
        result should equal (true)
    }

    test("sortCriteria - Empty") {
        val inList = Nil
        val resultList = TestingCore.sortCriteria(inList)
        val expected = Nil
        resultList should equal (expected)
    }

    test("sortCriteria - One value") {
        val inList = List(TestCriteriaValueTuple(1, RunTest))
        val resultList = TestingCore.sortCriteria(inList)
        val expected = inList
        resultList should equal (expected)
    }

    test("sortCriteria - One range") {
        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange))
        val resultList = TestingCore.sortCriteria(inList)
        val expected = inList
        resultList should equal (expected)
    }

    test("sortCriteria - Many values (presorted)") {
        val inList = List(TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(3, RunTest), TestCriteriaValueTuple(17, RunTest))
        val resultList = TestingCore.sortCriteria(inList)
        val expected = inList
        resultList should equal (expected)
    }

    test("sortCriteria - Many ranges (presorted)") {
        val inList = List(TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaRangeTuple(7, 7, RunRange),
                          TestCriteriaRangeTuple(8, 9, RunRange), TestCriteriaRangeTuple(12, 17, RunRange))
        val resultList = TestingCore.sortCriteria(inList)
        val expected = inList
        resultList should equal (expected)
    }

    test("sortCriteria - Many values (unsorted)") {
        val inList = List(TestCriteriaValueTuple(17, RunTest), TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(3, RunTest))
        val resultList = TestingCore.sortCriteria(inList)
        val expected = List(TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(3, RunTest), TestCriteriaValueTuple(17, RunTest))
        resultList should equal (expected)
    }

    test("sortCriteria - Many ranges (reversed)") {
        val inList = List(TestCriteriaRangeTuple(12, 17, RunRange), TestCriteriaRangeTuple(8, 9, RunRange),
                          TestCriteriaRangeTuple(7, 7, RunRange), TestCriteriaRangeTuple(1, 6, RunRange))
        val resultList = TestingCore.sortCriteria(inList)
        val expected = inList.reverse
        resultList should equal (expected)
    }

    test("bucketAListOn_2 - Empty") {
        val inList = Nil
        val resultList = TestingCore.bucketAListOn_2(inList)
        val expected = Array()
        resultList should equal (expected)
    }

    test("bucketAListOn_2 - One Char") {
        val inList = List(('a', 0))
        val resultList = TestingCore.bucketAListOn_2(inList)
        val expected = Array('a')
        resultList should equal (expected)
    }

    test("bucketAListOn_2 - Many chars") {
        val inList = List(('a', 0), ('s', 3), ('e', 2), ('o', 4), ('e', 6), ('w', 1), ('m', 5))
        val resultList = TestingCore.bucketAListOn_2(inList)
        val expected = "awesome"
        resultList.toList.mkString should equal (expected)
    }

    test("handleRanges - Empty") {
        val inList = Nil
        val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList)
        val expectedTests = Nil
        val expectedSkips = Nil
        val expectedMax = 0
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleRanges - One run") {
        val inList = List(TestCriteriaRangeTuple(1, 3, RunRange))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList)
        val expectedTests = inList
        val expectedSkips = Nil
        val expectedMax = 3
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }
    
    test("handleRanges - One skip") {
        val inList = List(TestCriteriaRangeTuple(1, 3, SkipRange))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList)
        val expectedTests = Nil
        val expectedSkips = inList
        val expectedMax = 0
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleRanges - Many runs") {
        val inList = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(4, 4, RunRange), TestCriteriaRangeTuple(5, 6, RunRange))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList)
        val expectedTests = inList
        val expectedSkips = Nil
        val expectedMax = 6
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleRanges - Many skips") {
        val inList = List(TestCriteriaRangeTuple(1, 3, SkipRange), TestCriteriaRangeTuple(4, 4, SkipRange), TestCriteriaRangeTuple(5, 6, SkipRange))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList)
        val expectedTests = Nil
        val expectedSkips = inList
        val expectedMax = 0
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleRanges - Many (mixed)") {
        val inList = List(TestCriteriaRangeTuple(1, 3, SkipRange), TestCriteriaRangeTuple(4, 4, RunRange), TestCriteriaRangeTuple(5, 6, SkipRange))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleRanges(inList)
        val expectedTests = List(TestCriteriaRangeTuple(4, 4, RunRange))
        val expectedSkips = List(TestCriteriaRangeTuple(1, 3, SkipRange), TestCriteriaRangeTuple(5, 6, SkipRange))
        val expectedMax = 4
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }
    
    test("handleValues - Empty") {
        val inList = Nil
        val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList)
        val expectedTests = Nil
        val expectedSkips = Nil
        val expectedMax = 0
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleValues - One run") {
        val inList = List(TestCriteriaValueTuple(1, RunTest))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList)
        val expectedTests = inList
        val expectedSkips = Nil
        val expectedMax = 1
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }
    
    test("handleValues - One skip") {
        val inList = List(TestCriteriaValueTuple(1, SkipTest))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList)
        val expectedTests = Nil
        val expectedSkips = inList
        val expectedMax = 0
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleValues - Many runs") {
        val inList = List(TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(4, RunTest), TestCriteriaValueTuple(5, RunTest))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList)
        val expectedTests = inList
        val expectedSkips = Nil
        val expectedMax = 5
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleValues - Many skips") {
        val inList = List(TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(4, SkipTest),  TestCriteriaValueTuple(5, SkipTest))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList)
        val expectedTests = Nil
        val expectedSkips = inList
        val expectedMax = 0
        resultTests should equal (expectedTests)
        resultSkips should equal (expectedSkips)
        resultMax   should equal (expectedMax)
    }

    test("handleValues - Many (mixed)") {
        val inList = List(TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(4, RunTest), TestCriteriaValueTuple(5, SkipTest))
        val (resultTests, resultSkips, resultMax) = TestingCore.handleValues(inList)
        val expectedTests = List(TestCriteriaValueTuple(4, RunTest))
        val expectedSkips = List(TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(5, SkipTest))
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

        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange))
        val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

        result === false
        offenderPart1 === None
        offenderPart2 === None

    }

    test("containsOverlaps - Many (without overlap)") {

        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(3, 7, RunRange), TestCriteriaRangeTuple(9, 9, RunRange))
        val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

        result === false
        offenderPart1 === None
        offenderPart2 === None

    }

    test("containsOverlaps - Many (with SOME overlap)") {

        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(3, 7, RunRange), TestCriteriaRangeTuple(7, 9, RunRange))
        val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

        result === true
        offenderPart1 === Some(TestCriteriaRangeTuple(3, 7, RunRange))
        offenderPart2 === Some(TestCriteriaRangeTuple(7, 9, RunRange))

    }

    test("containsOverlaps - Many (with ALL overlaps)") {

        val inList = List(TestCriteriaRangeTuple(1, 5, RunRange), TestCriteriaRangeTuple(3, 7, RunRange), TestCriteriaRangeTuple(7, 9, RunRange))
        val (result, offenderPart1, offenderPart2) = TestingCore.containsOverlaps(inList)

        result === true
        offenderPart1 === Some(TestCriteriaRangeTuple(1, 5, RunRange))
        offenderPart2 === Some(TestCriteriaRangeTuple(3, 7, RunRange))

    }

    test("generateResultArray - Empty, empty, empty, empty, 10") {
        val resultArr = TestingCore.generateResultArray(Nil, Nil, Nil, Nil, 10)
        val expected = Array(false, false, false, false, false,
                             false, false, false, false, false, false)
        resultArr should equal (expected)
    }

    test("generateResultArray - One, one, one, one, 10") {
        val resultArr = TestingCore.generateResultArray(List(TestCriteriaRangeTuple(2, 8, RunRange)), List(TestCriteriaValueTuple(10, RunTest)),
                                                        List(TestCriteriaRangeTuple(4, 5, SkipRange)), List(TestCriteriaValueTuple(7, SkipTest)), 10)
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
        val inValue = TestCriteriaValueTuple(1, RunTest)
        val resultMap = TestingCore.sortArgLists(List(inValue))
        val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
        resultValues should equal (List(inValue))
        resultRanges should equal (Nil)
        resultToggles should equal (Nil)
    }

    test("sortArgLists - One range") {
        val inRange = TestCriteriaRangeTuple(1, 1, RunRange)
        val resultMap = TestingCore.sortArgLists(List(inRange))
        val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
        resultValues should equal (Nil)
        resultRanges should equal (List(inRange))
        resultToggles should equal (Nil)
    }

    test("sortArgLists - One toggle") {
        val inToggle = TestCriteriaToggleFlag(SkipPathingTests)
        val resultMap = TestingCore.sortArgLists(List(inToggle))
        val (resultValues, resultRanges, resultToggles) = (resultMap(TestingCore.ArgKeyValue), resultMap(TestingCore.ArgKeyRange), resultMap(TestingCore.ArgKeyToggle))
        resultValues should equal (Nil)
        resultRanges should equal (Nil)
        resultToggles should equal (List(inToggle))
    }

    test("sortArgLists - Many mixed") {

        val inValue1 = TestCriteriaValueTuple(11, SkipTest)
        val inValue2 = TestCriteriaValueTuple(15, RunTest)

        val inRange1 = TestCriteriaRangeTuple(1, 6, RunRange)
        val inRange2 = TestCriteriaRangeTuple(2, 4, SkipRange)
        val inRange3 = TestCriteriaRangeTuple(10, 13, RunRange)

        val inToggle1 = TestCriteriaToggleFlag(SkipPathingTests)
        val inToggle2 = TestCriteriaToggleFlag(SkipPathingTests)

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

    test("siftOutTestsAndSkips - No runs, no skips") {
        val (result1, result2) = TestingCore.siftOutTestsAndSkips(Nil)
        result1 should equal (Nil)
        result2 should equal (Nil)
    }

    test("siftOutTestsAndSkips - One run, no skips") {
        val tuple = TestCriteriaRangeTuple(1, 1, RunRange)
        val (result1, result2) = TestingCore.siftOutTestsAndSkips(List(tuple))
        result1 should equal (List(tuple))
        result2 should equal (Nil)
    }

    test("siftOutTestsAndSkips - No runs, one skip") {
        val tuple = TestCriteriaRangeTuple(1, 1, SkipRange)
        val (result1, result2) = TestingCore.siftOutTestsAndSkips(List(tuple))
        result1 should equal (Nil)
        result2 should equal (List(tuple))
    }

    test("siftOutTestsAndSkips - One run, one skip") {
        val runTuple = TestCriteriaRangeTuple(1, 1, RunRange)
        val skipTuple = TestCriteriaRangeTuple(1, 1, SkipRange)
        val (result1, result2) = TestingCore.siftOutTestsAndSkips(List(runTuple, skipTuple))
        result1 should equal (List(runTuple))
        result2 should equal (List(skipTuple))
    }

}