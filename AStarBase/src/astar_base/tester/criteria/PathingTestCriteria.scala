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

class PathingTestCriteriaValueTuple(tuple: PathingTestTuple[Int, TestValueFlag]) extends PathingTestCriteriaTuple(tuple){
    def apply(guide: Int,  flag: TestValueFlag) : PathingTestCriteriaValueTuple = {
        new PathingTestCriteriaValueTuple(new PathingTestTuple(guide, flag))
    }
}

class PathingTestCriteriaRangeTuple(tuple: PathingTestTuple[(Int, Int), TestRangeFlag]) extends PathingTestCriteriaTuple(tuple) {

    def apply(guide: (Int, Int),  flag: TestRangeFlag) : PathingTestCriteriaRangeTuple = {
        new PathingTestCriteriaRangeTuple(new PathingTestTuple(guide, flag))
    }

    def apply(guideStart: Int, guideEnd: Int,  flag: TestRangeFlag) : PathingTestCriteriaRangeTuple = {
        new PathingTestCriteriaRangeTuple(new PathingTestTuple((guideStart, guideEnd), flag))
    }

    def isValid : Boolean = {
        criteria.guide._1 <= criteria.guide._2
    }

    def intersects(that: PathingTestCriteriaRangeTuple) : Boolean = {
        val thisRange = Range(criteria.guide._1, criteria.guide._2)
        val thatRange = Range(that.criteria.guide._1, that.criteria.guide._2)
        thisRange.intersect(thatRange).size > 0       // There's certainly a better way to do this
    }

    def encapsulates(that: PathingTestCriteriaRangeTuple) : Boolean = {
        val thisRange = Range(criteria.guide._1, criteria.guide._2)
        val thatRange = Range(that.criteria.guide._1, that.criteria.guide._2)
        thisRange containsSlice thatRange
    }

    implicit def rangeTupleToValueTupleList(that: PathingTestCriteriaRangeTuple) : List[PathingTestCriteriaValueTuple] = {
        Range(that.criteria.guide._1, that.criteria.guide._2).foldLeft (List[PathingTestCriteriaValueTuple]()) ( (acc, x) => PathingTestCriteriaValueTuple(x, that.criteria.flag) :: acc)
    }
    
}

class PathingTestCriteriaToggleFlag(criteriaFlag: TestToggleFlag) extends PathingTestCriteria(criteriaFlag)