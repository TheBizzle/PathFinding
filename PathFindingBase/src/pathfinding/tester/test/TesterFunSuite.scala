package pathfinding.tester.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import pathfinding.tester.TestingCore
import pathfinding.tester.criteria._
import pathfinding.tester.criteria.TestCriteriaRangeTuple._
import collection.immutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 7:22 PM
 */

class TesterFunSuite extends FunSuite with ShouldMatchers {

    test("handleTestIntervals - Empty, empty") {
        val inValues = Nil
        val inRanges = Nil
        val result = TestingCore.handleTestIntervals(inValues, inRanges)
        val expectedList = Nil
        result should equal (expectedList)
    }

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
        val inRanges = List(TestCriteriaRangeTuple(1, 5, RunRange), TestCriteriaRangeTuple(2, 3, SkipRange))
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

    test("sortCriteriaValues - Empty list") {
        val inList = Nil
        val result = TestingCore.sortCriteriaValues(inList)
        result should equal (inList)
    }

    test("sortCriteriaValues - Single-item list") {
        val inList = List(TestCriteriaValueTuple(1, RunTest))
        val result = TestingCore.sortCriteriaValues(inList)
        result should equal (inList)
    }

    test("sortCriteriaValues - Multi-item list (presorted)") {
        val inList = List(TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(2, RunTest))
        val result = TestingCore.sortCriteriaValues(inList)
        result should equal (inList)
    }

    test("sortCriteriaValues - Multi-item list (backwards)") {
        val inList = List(TestCriteriaValueTuple(2, RunTest), TestCriteriaValueTuple(1, RunTest))
        val result = TestingCore.sortCriteriaValues(inList)
        result should equal (inList.reverse)
    }

    test("sortCriteriaRanges - Empty list") {
        val inList = Nil
        val result = TestingCore.sortCriteriaRanges(inList)
        result should equal (inList)
    }

    test("sortCriteriaRanges - Single-item list") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange))
        val result = TestingCore.sortCriteriaRanges(inList)
        result should equal (inList)
    }

    test("sortCriteriaRanges - Multi-item list (presorted)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(3, 4, RunRange))
        val result = TestingCore.sortCriteriaRanges(inList)
        result should equal (inList)
    }

    test("sortCriteriaRanges - Multi-item list (backwards)") {
        val inList = List(TestCriteriaRangeTuple(3, 4, RunRange), TestCriteriaRangeTuple(1, 2, RunRange))
        val result = TestingCore.sortCriteriaRanges(inList)
        result should equal (inList.reverse)
    }

    test("findUnionOfIntervals - Empty") {
        val inList = Nil
        val result = TestingCore.findUnionOfIntervals(inList)
        result should equal (inList)
    }

    test("findUnionOfIntervals - One-item (runs)") {
        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange))
        val result = TestingCore.findUnionOfIntervals(inList)
        result should equal (inList)
    }

    test("findUnionOfIntervals - Two-item (run, skip)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(2, 3, SkipRange))
        val result = TestingCore.findUnionOfIntervals(inList)
        val expectedList = List(TestCriteriaRangeTuple(1, 1, RunRange))
        result should equal (expectedList)
    }

    test("findUnionOfIntervals - Multi-item (homogenously of runs)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(2, 3, RunRange),
                          TestCriteriaRangeTuple(5, 6, RunRange), TestCriteriaRangeTuple(9, 11, RunRange),
                          TestCriteriaRangeTuple(11, 14, RunRange))
        val result = TestingCore.findUnionOfIntervals(inList)
        val expectedList = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(5, 6, RunRange), TestCriteriaRangeTuple(9, 14, RunRange))
        result should equal (expectedList)
    }

    test("findUnionOfIntervals - Multi-item (mixed)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(2, 3, RunRange),
                          TestCriteriaRangeTuple(5, 6, RunRange), TestCriteriaRangeTuple(9, 11, RunRange),
                          TestCriteriaRangeTuple(11, 14, RunRange), TestCriteriaRangeTuple(10, 11, SkipRange),
                          TestCriteriaRangeTuple(11, 14, SkipRange), TestCriteriaRangeTuple(5, 5, SkipRange))
        val result = TestingCore.findUnionOfIntervals(inList)
        val expectedList = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(6, 6, RunRange), TestCriteriaRangeTuple(9, 9, RunRange))
        result should equal (expectedList)
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

    test("condenseCriteriaTupleList - Empty list") {
        val inList = Nil
        val result = TestingCore.condenseCriteriaTupleList(inList)
        result should equal (inList)
    }

    test("condenseCriteriaTupleList - Single-item list") {
        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange))
        val result = TestingCore.condenseCriteriaTupleList(inList)
        result should equal (inList)
    }

    test("condenseCriteriaTupleList - Two-item list (unmergable)") {
        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(2, 2, RunRange))
        val result = TestingCore.condenseCriteriaTupleList(inList)
        result should equal (inList)
    }

    test("condenseCriteriaTupleList - Two-item list (mergable)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(2, 3, RunRange))
        val result = TestingCore.condenseCriteriaTupleList(inList)
        val expectedList = List(TestCriteriaRangeTuple(1, 3, RunRange))
        result should equal (expectedList)
    }

    test("condenseCriteriaTupleList - Three-item list (unmergable)") {
        val inList = List(TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(2, 2, RunRange), TestCriteriaRangeTuple(3, 3, RunRange))
        val result = TestingCore.condenseCriteriaTupleList(inList)
        result should equal (inList)
    }

    test("condenseCriteriaTupleList - Three-item list (semi-mergable)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(2, 3, RunRange), TestCriteriaRangeTuple(4, 4, RunRange))
        val result = TestingCore.condenseCriteriaTupleList(inList)
        val expectedList = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(4, 4, RunRange))
        result should equal (expectedList)
    }

    test("condenseCriteriaTupleList - Three-item list (fully mergable)") {
        val inList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(2, 5, RunRange), TestCriteriaRangeTuple(5, 10, RunRange))
        val result = TestingCore.condenseCriteriaTupleList(inList)
        val expectedList = List(TestCriteriaRangeTuple(1, 10, RunRange))
        result should equal (expectedList)
    }

    test("coalesceLists - Empty, empty") {
        val a = Nil
        val b = Nil
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = Nil
        result should equal (expectedList)
    }

    test("coalesceLists - One-item, empty") {
        val a = List(TestCriteriaRangeTuple(1, 1, RunRange))
        val b = Nil
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = a
        result should equal (expectedList)
    }

    test("coalesceLists - One-item, one-item (ah encapsulates bh)") {
        val a = List(TestCriteriaRangeTuple(1, 3, RunRange))
        val b = List(TestCriteriaRangeTuple(2, 2, SkipRange))
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = List(TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(3, 3, RunRange))
        result should equal (expectedList)
    }

    test("coalesceLists - One-item, one-item (ah intersects bh without bh encapsulating ah)") {
        val a = List(TestCriteriaRangeTuple(1, 3, RunRange))
        val b = List(TestCriteriaRangeTuple(3, 4, SkipRange))
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = List(TestCriteriaRangeTuple(1, 2, RunRange))
        result should equal (expectedList)
    }

    test("coalesceLists - Multi-item, multi-item (ah encapsulates bh)") {
        val a = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(5, 10, RunRange))
        val b = List(TestCriteriaRangeTuple(2, 2, SkipRange), TestCriteriaRangeTuple(6, 8, SkipRange))
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = List(TestCriteriaRangeTuple(1, 1, RunRange), TestCriteriaRangeTuple(3, 3, RunRange), TestCriteriaRangeTuple(5, 5, RunRange), TestCriteriaRangeTuple(9, 10, RunRange))
        result should equal (expectedList)
    }

    test("coalesceLists - Multi-item, multi-item (ah intersects bh without bh encapsulating ah)") {
        val a = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(5, 10, RunRange))
        val b = List(TestCriteriaRangeTuple(3, 4, SkipRange), TestCriteriaRangeTuple(8, 12, SkipRange))
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(5, 7, RunRange))
        result should equal (expectedList)
    }

    test("coalesceLists - Multi-item, multi-item (ah intersects bh without bh encapsulating ah, but bh extends into the next range)") {
        val a = List(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(5, 10, RunRange))
        val b = List(TestCriteriaRangeTuple(3, 5, SkipRange), TestCriteriaRangeTuple(7, 7, SkipRange), TestCriteriaRangeTuple(10, 17, SkipRange))
        val result = TestingCore.coalesceLists(a, b)
        val expectedList = List(TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(6, 6, RunRange), TestCriteriaRangeTuple(8, 9, RunRange))
        result should equal (expectedList)
    }

    test("mergeCriteriaRangeTuples - Call with two range tuples") {
        val result = TestingCore.mergeCriteriaRangeTuples(TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(2, 4, RunRange))
        result should equal (TestCriteriaRangeTuple(1, 4, RunRange))
    }

    test("createArrayFromRangeCriteriaList - Call with empty list") {
        val inList = Nil
        val result = TestingCore.createArrayFromRangeCriteriaList(inList, 20)
        val expectedArr = new Array[Boolean](21)
        result should equal (expectedArr)
    }

    test("createArrayFromRangeCriteriaList - Call with a single-item List[List[TestCriteriaValueTuple]]") {
        val inList = List(List(TestCriteriaValueTuple(1, RunTest)))
        val result = TestingCore.createArrayFromRangeCriteriaList(inList, 20)
        val expectedArr = new Array[Boolean](21)
        expectedArr(1) = true
        result should equal (expectedArr)
    }

    test("createArrayFromRangeCriteriaList - Call with a multi-item List[List[TestCriteriaValueTuple]]") {
        val inList = List(List(TestCriteriaValueTuple(1, RunTest)), List(TestCriteriaValueTuple(2, RunTest), TestCriteriaValueTuple(17, RunTest)))
        val result = TestingCore.createArrayFromRangeCriteriaList(inList, 20)
        val expectedArr = new Array[Boolean](21)
        expectedArr(1) = true
        expectedArr(2) = true
        expectedArr(17) = true
        result should equal (expectedArr)
    }

    test("createArrayFromRangeCriteriaList - Test 4: Call with a single-item List[TestCriteriaRangeTuple]") {
        val inList = List(TestCriteriaRangeTuple(1, 3, RunRange))
        val result = TestingCore.createArrayFromRangeCriteriaList(inList, 20)
        val expectedArr = new Array[Boolean](21)
        expectedArr(1) = true
        expectedArr(2) = true
        expectedArr(3) = true
        result should equal (expectedArr)
    }

    test("createArrayFromRangeCriteriaList - Test 4: Call with a multi-item List[TestCriteriaRangeTuple]") {
        val inList = List(TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaRangeTuple(10, 10, RunRange), TestCriteriaRangeTuple(13, 17, RunRange))
        val result = TestingCore.createArrayFromRangeCriteriaList(inList, 20)
        val expectedArr = new Array[Boolean](21)
        expectedArr(1) = true
        expectedArr(2) = true
        expectedArr(3) = true
        expectedArr(4) = true
        expectedArr(5) = true
        expectedArr(6) = true
        expectedArr(10) = true
        expectedArr(13) = true
        expectedArr(14) = true
        expectedArr(15) = true
        expectedArr(16) = true
        expectedArr(17) = true
        result should equal (expectedArr)
    }

    test("createArrayFromRangeCriteriaList - Test 4: Call with a mixed list") {
        val inList = List[List[TestCriteriaValueTuple]](List(TestCriteriaValueTuple(1, RunTest)), TestCriteriaRangeTuple(2, 6, RunRange), List(TestCriteriaValueTuple(9, RunTest), TestCriteriaValueTuple(17, RunTest)), TestCriteriaRangeTuple(10, 10, RunRange))
        val result = TestingCore.createArrayFromRangeCriteriaList(inList, 20)
        val expectedArr = new Array[Boolean](21)
        expectedArr(1) = true
        expectedArr(2) = true
        expectedArr(3) = true
        expectedArr(4) = true
        expectedArr(5) = true
        expectedArr(6) = true
        expectedArr(9) = true
        expectedArr(10) = true
        expectedArr(17) = true
        result should equal (expectedArr)
    }

    test("mergeValuesIntoArr - Empty list") {
        val testArr = Array(false, true, false, false, true, true)
        val arrCopy = testArr.clone()
        val result = TestingCore.mergeValuesIntoArr(arrCopy, Nil)
        result should equal (testArr)
    }

    test("mergeValuesIntoArr - One-run list") {
        val testArr = Array(false, true, false, false, true, true)
        val arrCopy = testArr.clone()
        val result = TestingCore.mergeValuesIntoArr(arrCopy, List(TestCriteriaValueTuple(2, RunTest)))
        testArr(2) = true
        result should equal (testArr)
    }

    test("mergeValuesIntoArr - One-skip list") {
        val testArr = Array(false, true, false, false, true, true)
        val arrCopy = testArr.clone()
        val result = TestingCore.mergeValuesIntoArr(arrCopy, List(TestCriteriaValueTuple(1, SkipTest)))
        testArr(1) = false
        result should equal (testArr)
    }

    test("mergeValuesIntoArr - Multi-item list (homogenously of runs)") {
        val testArr = Array(false, true, false, false, true, true)
        val arrCopy = testArr.clone()
        val result = TestingCore.mergeValuesIntoArr(arrCopy, List(TestCriteriaValueTuple(2, RunTest), TestCriteriaValueTuple(3, RunTest)))
        testArr(2) = true
        testArr(3) = true
        result should equal (testArr)
    }

    test("mergeValuesIntoArr - Multi-item list (homogenously of skips)") {
        val testArr = Array(false, true, false, false, true, true)
        val arrCopy = testArr.clone()
        val result = TestingCore.mergeValuesIntoArr(arrCopy, List(TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(4, SkipTest)))
        testArr(1) = false
        testArr(4) = false
        result should equal (testArr)
    }

    test("mergeValuesIntoArr - Multi-item list (mixed)") {
        val testArr = Array(false, true, false, false, true, true)
        val arrCopy = testArr.clone()
        val result = TestingCore.mergeValuesIntoArr(arrCopy, List(TestCriteriaValueTuple(2, RunTest), TestCriteriaValueTuple(3, RunTest),
                                                                  TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(4, SkipTest)))
        testArr(1) = false
        testArr(2) = true
        testArr(3) = true
        testArr(4) = false
        result should equal (testArr)
    }

}