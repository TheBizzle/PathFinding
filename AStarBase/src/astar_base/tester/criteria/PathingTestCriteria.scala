package astar_base.tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:30 PM
 */

sealed abstract class PathingTestCriteria[T](criteriaValue: T) {
    val criteria = criteriaValue
}

sealed class PathingTestTuple[T, U <: PathingTestingFlag](testGuide: T, testFlag: U) {
    val guide = testGuide
    val flag = testFlag
}

sealed abstract class PathingTestCriteriaTuple[T, U <: PathingTestingFlag](criteriaTuple: PathingTestTuple[T, U]) extends PathingTestCriteria(criteriaTuple)
class PathingTestCriteriaValueTuple(tuple: PathingTestTuple[Int, TestValueFlag]) extends PathingTestCriteriaTuple(tuple)
class PathingTestCriteriaRangeTuple(tuple: PathingTestTuple[(Int, Int), TestRangeFlag]) extends PathingTestCriteriaTuple(tuple)
class PathingTestCriteriaToggleFlag(criteriaFlag: TestToggleFlag) extends PathingTestCriteria(criteriaFlag)