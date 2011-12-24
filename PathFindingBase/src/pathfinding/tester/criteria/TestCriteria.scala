package pathfinding.tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:30 PM
 */

// ==========================================+-----------------------+============================================
//                                           |   Sealed abstracts    |
// ==========================================+-----------------------+============================================

sealed abstract class TestCriteria[T](criteriaValue: T) {
    val criteria = criteriaValue
}

sealed class TestTuple[T, U <: ArityTestingFlag with RunningnessTestingFlag](testGuide: T, testFlag: U) {
    val guide = testGuide
    val flag = testFlag
}

sealed abstract class TestCriteriaTuple[T, U <: ArityTestingFlag with RunningnessTestingFlag](criteriaTuple: TestTuple[T, U]) extends TestCriteria(criteriaTuple)

// ==========================================+-----------------------+============================================
//                                           |     Value tuple       |
// ==========================================+-----------------------+============================================

class TestCriteriaValueTuple(tuple: TestTuple[Int, TestValueFlag with RunningnessTestingFlag]) extends TestCriteriaTuple(tuple)

object TestCriteriaValueTuple {
    def apply(guide: Int,  flag: TestValueFlag with RunningnessTestingFlag) : TestCriteriaValueTuple = {
        new TestCriteriaValueTuple(new TestTuple(guide, flag))
    }
}

// ==========================================+-----------------------+============================================
//                                           |     Range tuple       |
// ==========================================+-----------------------+============================================

class TestCriteriaRangeTuple(tuple: TestTuple[(Int, Int), TestRangeFlag with RunningnessTestingFlag]) extends TestCriteriaTuple(tuple) {

    def isValid : Boolean = {
        criteria.guide._1 <= criteria.guide._2
    }

    def intersects(that: TestCriteriaRangeTuple) : Boolean = {
        val thisRange = Range(criteria.guide._1, criteria.guide._2)
        val thatRange = Range(that.criteria.guide._1, that.criteria.guide._2)
        thisRange.intersect(thatRange).size > 0       // There's certainly a better way to do this
    }

    def encapsulates(that: TestCriteriaRangeTuple) : Boolean = {
        val thisRange = Range(criteria.guide._1, criteria.guide._2)
        val thatRange = Range(that.criteria.guide._1, that.criteria.guide._2)
        thisRange containsSlice thatRange
    }

}

object TestCriteriaRangeTuple {
    def apply(guide: (Int, Int),  flag: TestRangeFlag with RunningnessTestingFlag) : TestCriteriaRangeTuple = {
        new TestCriteriaRangeTuple(new TestTuple(guide, flag))
    }
    def apply(guideStart: Int, guideEnd: Int,  flag: TestRangeFlag with RunningnessTestingFlag) : TestCriteriaRangeTuple = {
        new TestCriteriaRangeTuple(new TestTuple((guideStart, guideEnd), flag))
    }
    implicit def rangeTupleListToValueTupleListList(that: List[TestCriteriaRangeTuple]) : List[List[TestCriteriaValueTuple]] = {
        that map ( rangeTupleToValueTupleList(_) )
    }
    implicit def rangeTupleToValueTupleList(that: TestCriteriaRangeTuple) : List[TestCriteriaValueTuple] = {
        Range(that.criteria.guide._1, that.criteria.guide._2).foldLeft (List[TestCriteriaValueTuple]()) ( (acc, x) => TestCriteriaValueTuple(x, TestingFlag.flipArity(that.criteria.flag)) :: acc)
    }
}

// ==========================================+-----------------------+============================================
//                                           |     Toggle tuple      |
// ==========================================+-----------------------+============================================

class TestCriteriaToggleFlag(criteriaFlag: TestToggleFlag) extends TestCriteria(criteriaFlag)

object TestCriteriaToggleFlag {
    def apply(flag: TestToggleFlag) : TestCriteriaToggleFlag = {
        new TestCriteriaToggleFlag(flag)
    }
}