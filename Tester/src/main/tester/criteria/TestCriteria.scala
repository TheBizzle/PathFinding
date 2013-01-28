package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:30 PM
 */

// This whole file is a piece of shit. --Jason

sealed trait TestCriteria

abstract class TestFlagCriteria[T <: TestingFlag](testFlag: T) extends TestCriteria {
  val flag = testFlag
}

sealed abstract class TestRunningnessCriteria[T, U <: TestRunningnessFlag](testGuide: T, testFlag: U) extends TestFlagCriteria(testFlag) with Equals {
  val guide = testGuide
  def getKey : Int
}



class TestRunningnessValue(testGuide: Int, testFlag: TestRunningnessFlag) extends TestRunningnessCriteria(testGuide, testFlag) {

  override def getKey   = guide
  override def toString = "(%s)".format(guide)

  override def hashCode            =  41 * (4111 + guide) + flag.hashCode()
  override def canEqual(that: Any) = that.isInstanceOf[TestRunningnessValue]
  override def equals  (that: Any) =
    that match {
      case thatValueTuple: TestRunningnessValue => (thatValueTuple canEqual this) && (guide == thatValueTuple.guide && (flag == thatValueTuple.flag))
      case _                                    => false
    }

}

object TestRunningnessValue {
  def apply(guide: Int,  flag: TestRunningnessFlag) = new TestRunningnessValue(guide, flag)
}



class TestRunningnessRange(testBounds: (Int, Int), testFlag: TestRunningnessFlag) extends TestRunningnessCriteria(testBounds, testFlag) {

  def isValid = guide._1 <= guide._2

  def intersects(that: TestRunningnessRange) : Boolean = {
    val thisRange = Range(guide._1, guide._2)
    val thatRange = Range(that.guide._1, that.guide._2)
    thisRange.intersect(thatRange).size > 0  // There's certainly a better way to do this (true... but writing out the boolean gymnastics for it just isn't fun)
  }

  def encapsulates(that: TestRunningnessRange) : Boolean = {
    val thisRange = Range(guide._1, guide._2)
    val thatRange = Range(that.guide._1, that.guide._2)
    thisRange containsSlice thatRange
  }

  override def getKey   = guide._1
  override def toString = "(%s, %s)".format(guide._1, guide._2)

  override def hashCode            = 41 * (41 * (4111 + guide._1) + guide._2) + flag.hashCode()
  override def canEqual(that: Any) = that.isInstanceOf[TestRunningnessRange]
  override def equals(that: Any)   =
    that match {
      case thatRangeTuple: TestRunningnessRange => (thatRangeTuple canEqual this) && (guide._1 == thatRangeTuple.guide._1
                                                                                  && (guide._2 == thatRangeTuple.guide._2)
                                                                                  && (flag == thatRangeTuple.flag))
      case _ => false
    }

}

object TestRunningnessRange {

  def apply(guide: (Int, Int), flag: TestRunningnessFlag)               = new TestRunningnessRange(guide, flag)
  def apply(guideStart: Int, guideEnd: Int,  flag: TestRunningnessFlag) = new TestRunningnessRange((guideStart, guideEnd), flag)

  implicit def rangeTupleSet2ValueTupleSetSet(ranges: Set[TestRunningnessRange]) : Set[Set[TestRunningnessValue]] = ranges map (rangeTuple2ValueTupleSet(_))
  implicit def rangeTuple2ValueTupleSet(range: TestRunningnessRange)             : Set[TestRunningnessValue]      =
    Range(range.guide._1, range.guide._2).inclusive map (TestRunningnessValue(_, range.flag)) toSet

}



class TestCriteriaToggleFlag(criteriaFlag: TestToggleFlag) extends TestFlagCriteria(criteriaFlag) with Equals {
  override def hashCode            = 4111 + flag.hashCode
  override def canEqual(that: Any) = that.isInstanceOf[TestCriteriaToggleFlag]
  override def equals(that: Any) =
    that match {
      case thatToggle: TestCriteriaToggleFlag => (thatToggle canEqual this) && (flag == thatToggle.flag)
      case _                                  => false
    }
}

object TestCriteriaToggleFlag {
  def apply(flag: TestToggleFlag) = new TestCriteriaToggleFlag(flag)
}
