package tester

import
  tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/15/12
 * Time: 12:04 PM
 */

object TestCriteriaDialect {
  implicit def int2PTCVal(value: Int)                                     : CombinatorPimper    = CombinatorPimper(TestRunningnessValue(value, RunTest))
  implicit def criteriaFlag2PTCFlag[T <% TestCriteriaToggleFlag](flag: T) : CombinatorPimper    = CombinatorPimper(flag)
  implicit def int2PTCPromoter(value: Int)                                : PimpedClassPromoter = PimpedClassPromoter(value)
  implicit def pimper2TCSeq(pimper: CombinatorPimper)                     : Seq[TestCriteria]   = pimper.^^  // Might need to get rid of this at some point...
}

private[tester] case class CombinatorPimper(crit: TestCriteria, tail: Seq[TestCriteria] = Seq()) {
  def &&(that: CombinatorPimper) : CombinatorPimper  = CombinatorPimper(crit, tail ++ (that.crit +: that.tail))
  def ^^                         : Seq[TestCriteria] = crit +: tail
}

// This class handles the class promotion of modified values, while barring invalid modifier combinations
// i.e. `!13` is allowed (since it makes total sense), but `!13 >!> 18` is not.
private[tester] case class PimpedClassPromoter(value: Int) {
  def unary_!       = CombinatorPimper(TestRunningnessValue(value, SkipTest))
  def >&>(end: Int) = CombinatorPimper(TestRunningnessRange(value, end, RunTest))
  def >!>(end: Int) = CombinatorPimper(TestRunningnessRange(value, end, SkipTest))
}
