package tester

import
  collection.mutable.ListBuffer

import
  tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/15/12
 * Time: 12:04 PM
 */

object TestCriteriaDialect {
  implicit def int2PTCVal(value: Int)                                     : PimpedValueTuple    = PimpedValueTuple(TestRunningnessValue(value, RunTest))
  implicit def int2PTCPromoter(value: Int)                                : PimpedClassPromoter = PimpedClassPromoter(value)
  implicit def criteriaFlag2PTCFlag[T <% TestCriteriaToggleFlag](flag: T) : PimpedToggleFlag    = PimpedToggleFlag(flag)
  implicit def pimper2TCSeq(pimper: CombinatorPimper)                     : Seq[TestCriteria]   = pimper.^^  // Might need to get rid of this at some point...
}


// Utility classes

// This feels a little weird, but it's unclear how best to solve the problem at hand here.
// Either a single item must actually be a sequence of items, or, when things start to
// chain, the single item needs to morph into a sequence of items that behaves similarly
// to how the single item does in the first place.  So... it seems that I either have
// to have weird, unclear, duplicated code, or weird, identity-crisis-suffering classes.
// I've chosen the latter.

//@ This could be substantially more FP (by having making constructor take form: Cons(crit, tail)
//  and having `&&` return `new CombinatorPimper(crit, tail ::: (that.crit :: that.tail))`)
//  Could even leverage HList-like static typing on the list, then....
//  This deserves to be fixed at some point.
private[tester] sealed abstract class CombinatorPimper(private val crit: TestCriteria) {

  private[tester] val buffer = new ListBuffer[TestCriteria]() += crit

  def &&(that: CombinatorPimper) : CombinatorPimper  = { buffer ++= that.buffer; this }
  def ^^                         : Seq[TestCriteria] =   buffer.toSeq

}

// With the current state of things, the necessity of these pimped classes is actually questionable
// Couldn't I just get rid of these altogether and directly instantiate `CombinatorPimper`s?
// But is that a good idea...?
private[tester] case class PimpedValueTuple(valueTuple: TestRunningnessValue) extends CombinatorPimper(valueTuple)
private[tester] case class PimpedRangeTuple(rangeTuple: TestRunningnessRange) extends CombinatorPimper(rangeTuple)
private[tester] case class PimpedToggleFlag(toggleFlag: TestCriteriaToggleFlag) extends CombinatorPimper(toggleFlag)

// This class handles the class promotion of modified values, while barring invalid modifier combinations
// i.e. `!13` is allowed (since it makes total sense), but `!13 >!> 18` is not.
private[tester] case class PimpedClassPromoter(value: Int) {
  def unary_!       : PimpedValueTuple = PimpedValueTuple(TestRunningnessValue(value, SkipTest))
  def >&>(end: Int) : PimpedRangeTuple = PimpedRangeTuple(TestRunningnessRange(value, end, RunTest))
  def >!>(end: Int) : PimpedRangeTuple = PimpedRangeTuple(TestRunningnessRange(value, end, SkipTest))
}
