package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:30 PM
 */

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|  Sealed base classes  |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

sealed abstract class TestCriteria[T](criteriaValue: T) {
    val criteria = criteriaValue
}

sealed class TestTuple[T, U <: TestRunningnessFlag](testGuide: T, testFlag: U) {
    val guide = testGuide
    val flag = testFlag
}

sealed abstract class TestCriteriaTuple[T, U <: TestRunningnessFlag](criteriaTuple: TestTuple[T, U]) extends TestCriteria(criteriaTuple) {
    def getKey : Int
}

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|     Value tuple       |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

class TestCriteriaValueTuple(tuple: TestTuple[Int, TestRunningnessFlag]) extends TestCriteriaTuple(tuple) {

    def getKey : Int = {
        criteria.guide
    }

    override def toString : String = {
        "(" + criteria.guide + ")"
    }

    override def equals(that: Any) : Boolean = {
        that match {
            case thatValueTuple: TestCriteriaValueTuple => (thatValueTuple canEqual this) && (criteria.guide == thatValueTuple.criteria.guide
                                                                                          && (criteria.flag == thatValueTuple.criteria.flag))
            case _ => false
        }
    }

    def canEqual(that: Any) : Boolean = {
        that.isInstanceOf[TestCriteriaValueTuple]
    }

    override def hashCode : Int = {
        41 * (4111 + criteria.guide) + criteria.flag.hashCode()
    }

}

object TestCriteriaValueTuple {
    def apply(guide: Int,  flag: TestRunningnessFlag) : TestCriteriaValueTuple = {
        new TestCriteriaValueTuple(new TestTuple(guide, flag))
    }
}

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|     Range tuple       |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

class TestCriteriaRangeTuple(tuple: TestTuple[(Int, Int), TestRunningnessFlag]) extends TestCriteriaTuple(tuple) {

    def getKey : Int = {
        criteria.guide._1
    }

    override def toString : String = {
        "(" + criteria.guide._1 + ", " + criteria.guide._2 + ")"
    }

    def isValid : Boolean = {
        criteria.guide._1 <= criteria.guide._2
    }

    def intersects(that: TestCriteriaRangeTuple) : Boolean = {
        val thisRange = Range(criteria.guide._1, criteria.guide._2)
        val thatRange = Range(that.criteria.guide._1, that.criteria.guide._2)
        thisRange.intersect(thatRange).size > 0       // There's certainly a better way to do this (true... but writing out the boolean gymnastics for it just isn't fun)
    }

    def encapsulates(that: TestCriteriaRangeTuple) : Boolean = {
        val thisRange = Range(criteria.guide._1, criteria.guide._2)
        val thatRange = Range(that.criteria.guide._1, that.criteria.guide._2)
        thisRange containsSlice thatRange
    }

    override def equals(that: Any) : Boolean = {
        that match {
            case thatRangeTuple: TestCriteriaRangeTuple => (thatRangeTuple canEqual this) && (criteria.guide._1 == thatRangeTuple.criteria.guide._1
                                                                                          && (criteria.guide._2 == thatRangeTuple.criteria.guide._2)
                                                                                          && (criteria.flag == thatRangeTuple.criteria.flag))
            case _ => false
        }
    }

    def canEqual(that: Any) : Boolean = {
        that.isInstanceOf[TestCriteriaRangeTuple]
    }

    override def hashCode : Int = {
        41 * (41 * (4111 + criteria.guide._1) + criteria.guide._2) + criteria.flag.hashCode()
    }

}

object TestCriteriaRangeTuple {
    def apply(guide: (Int, Int),  flag: TestRunningnessFlag) : TestCriteriaRangeTuple = {
        new TestCriteriaRangeTuple(new TestTuple(guide, flag))
    }
    def apply(guideStart: Int, guideEnd: Int,  flag: TestRunningnessFlag) : TestCriteriaRangeTuple = {
        new TestCriteriaRangeTuple(new TestTuple((guideStart, guideEnd), flag))
    }
    implicit def rangeTupleListToValueTupleListList(that: List[TestCriteriaRangeTuple]) : List[List[TestCriteriaValueTuple]] = {
        that map ( rangeTupleToValueTupleList(_) )
    }
    implicit def rangeTupleToValueTupleList(that: TestCriteriaRangeTuple) : List[TestCriteriaValueTuple] = {
        Range(that.criteria.guide._1, that.criteria.guide._2).inclusive.foldLeft(List[TestCriteriaValueTuple]()){ case (acc, x) => TestCriteriaValueTuple(x, that.criteria.flag) :: acc }
    }
}

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|     Toggle tuple      |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

class TestCriteriaToggleFlag(criteriaFlag: TestToggleFlag) extends TestCriteria(criteriaFlag) {

    override def equals(that: Any) : Boolean = {
        that match {
            case thatToggle: TestCriteriaToggleFlag => (thatToggle canEqual this) && (criteria == thatToggle.criteria)
            case _ => false
        }
    }

    override def hashCode : Int = {
        (4111 + criteria.hashCode())
    }

    def canEqual(that: Any) : Boolean = {
        that.isInstanceOf[TestCriteriaToggleFlag]
    }

}

object TestCriteriaToggleFlag {
    def apply(flag: TestToggleFlag) : TestCriteriaToggleFlag = {
        new TestCriteriaToggleFlag(flag)
    }
}