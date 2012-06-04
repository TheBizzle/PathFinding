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

sealed trait TestCriteria

abstract class TestFlagCriteria[T <: TestingFlag](testFlag: T) extends TestCriteria {
  val flag = testFlag
}

sealed abstract class TestRunningnessCriteria[T, U <: TestRunningnessFlag](testGuide: T, testFlag: U) extends TestFlagCriteria(testFlag) with Equals {
  val guide = testGuide
  def getKey : Int
}

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|     Value tuple       |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

class TestRunningnessValue(testGuide: Int, testFlag: TestRunningnessFlag) extends TestRunningnessCriteria(testGuide, testFlag) {

  def getKey : Int = {
    guide
  }

  override def toString : String = {
    "(" + guide + ")"
  }

  override def equals(that: Any) : Boolean = {
    that match {
      case thatValueTuple: TestRunningnessValue => (thatValueTuple canEqual this) && (guide == thatValueTuple.guide && (flag == thatValueTuple.flag))
      case _                                    => false
    }
  }

  def canEqual(that: Any) : Boolean = {
    that.isInstanceOf[TestRunningnessValue]
  }

  override def hashCode : Int = {
    41 * (4111 + guide) + flag.hashCode()
  }

}

object TestRunningnessValue {
  def apply(guide: Int,  flag: TestRunningnessFlag) : TestRunningnessValue = new TestRunningnessValue(guide, flag)
}

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|     Range tuple       |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

class TestRunningnessRange(testBounds: (Int, Int), testFlag: TestRunningnessFlag) extends TestRunningnessCriteria(testBounds, testFlag) {

  def getKey : Int = {
    guide._1
  }

  override def toString : String = {
    "(" + guide._1 + ", " + guide._2 + ")"
  }

  def isValid : Boolean = {
    guide._1 <= guide._2
  }

  def intersects(that: TestRunningnessRange) : Boolean = {
    val thisRange = Range(guide._1, guide._2)
    val thatRange = Range(that.guide._1, that.guide._2)
    thisRange.intersect(thatRange).size > 0       // There's certainly a better way to do this (true... but writing out the boolean gymnastics for it just isn't fun)
  }

  def encapsulates(that: TestRunningnessRange) : Boolean = {
    val thisRange = Range(guide._1, guide._2)
    val thatRange = Range(that.guide._1, that.guide._2)
    thisRange containsSlice thatRange
  }

  override def equals(that: Any) : Boolean = {
    that match {
      case thatRangeTuple: TestRunningnessRange => (thatRangeTuple canEqual this) && (guide._1 == thatRangeTuple.guide._1
                                                                                  && (guide._2 == thatRangeTuple.guide._2)
                                                                                  && (flag == thatRangeTuple.flag))
      case _ => false
    }
  }

  def canEqual(that: Any) : Boolean = {
    that.isInstanceOf[TestRunningnessRange]
  }

  override def hashCode : Int = {
    41 * (41 * (4111 + guide._1) + guide._2) + flag.hashCode()
  }

}

object TestRunningnessRange {
  def apply(guide: (Int, Int),  flag: TestRunningnessFlag) : TestRunningnessRange = {
    new TestRunningnessRange(guide, flag)
  }
  def apply(guideStart: Int, guideEnd: Int,  flag: TestRunningnessFlag) : TestRunningnessRange = {
    new TestRunningnessRange((guideStart, guideEnd), flag)
  }
  implicit def rangeTupleListToValueTupleListList(that: List[TestRunningnessRange]) : List[List[TestRunningnessValue]] = {
    that map (rangeTupleToValueTupleList(_))
  }
  implicit def rangeTupleToValueTupleList(that: TestRunningnessRange) : List[TestRunningnessValue] = {
    Range(that.guide._1, that.guide._2).inclusive map (TestRunningnessValue(_, that.flag)) toList
  }
}

// ==========================================+-----------------------+============================================
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|     Toggle tuple      |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// ==========================================+-----------------------+============================================

class TestCriteriaToggleFlag(criteriaFlag: TestToggleFlag) extends TestFlagCriteria(criteriaFlag) {

  override def equals(that: Any) : Boolean = {
    that match {
      case thatToggle: TestCriteriaToggleFlag => (thatToggle canEqual this) && (flag == thatToggle.flag)
      case _                                  => false
    }
  }

  override def hashCode : Int = {
    (4111 + flag.hashCode())
  }

  def canEqual(that: Any) : Boolean = {
    that.isInstanceOf[TestCriteriaToggleFlag]
  }

}

object TestCriteriaToggleFlag {
  def apply(flag: TestToggleFlag) : TestCriteriaToggleFlag = new TestCriteriaToggleFlag(flag)
}
