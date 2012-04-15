package tester.testscript.dialect

import collection.mutable.ListBuffer
import tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/15/12
 * Time: 12:04 PM
 */

object TestCriteriaDialect {
  implicit def int2PTCVal(value: Int)                                     : PimpedValueTuple = PimpedValueTuple(TestRunningnessValue(value, RunTest))
  implicit def criteriaFlag2PTCFlag[T <% TestCriteriaToggleFlag](flag: T) : PimpedToggleFlag = PimpedToggleFlag(flag)
  implicit def pimper2TCList(pimper: CombinatorPimper) : List[TestCriteria]                  = pimper.^^  // Might need to get rid of this at some point...
}


// Utility classes

// This feels a little weird, but it's unclear how best to solve the problem at hand here.
// Either a single item must actually be a sequence of items, or, when things start to
// chain, the single item needs to morph into a sequence of items that behaves similarly
// to how the single item does in the first place.  So... it seems that I either have
// to have weird, unclear, duplicated code, or weird, identity-crisis-suffering classes.
// I've chosen the latter.
private[dialect] sealed abstract class CombinatorPimper(private val crit: TestCriteria) {
  
  private[dialect] val buffer = new ListBuffer[TestCriteria]() += crit

  def &&(that: CombinatorPimper) : CombinatorPimper   = { buffer ++= that.buffer; this }
  def ^^                         : List[TestCriteria] =   buffer.toList

}

private[dialect] case class PimpedRangeTuple(rangeTuple: TestRunningnessRange) extends CombinatorPimper(rangeTuple)
private[dialect] case class PimpedToggleFlag(toggleFlag: TestCriteriaToggleFlag) extends CombinatorPimper(toggleFlag)
private[dialect] case class PimpedValueTuple(valueTuple: TestRunningnessValue) extends CombinatorPimper(valueTuple) {
  // This design kind of sucks, but I don't feel like there's much I can do about it...
  def unary_!       : PimpedValueTuple = PimpedValueTuple(TestRunningnessValue(valueTuple.guide, TestingFlag.flipRunningness(valueTuple.flag)))
  def >&>(end: Int) : PimpedRangeTuple = PimpedRangeTuple(TestRunningnessRange(valueTuple.guide, end, RunTest))
  def >!>(end: Int) : PimpedRangeTuple = PimpedRangeTuple(TestRunningnessRange(valueTuple.guide, end, SkipTest))
}
