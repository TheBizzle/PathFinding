package pathfinding.testscript

import pathfinding.testcluster.PathingTestCluster
import tester.TestingCore
import pathfinding.{StepData, PathFinder}
import tester.criteria._
import shapeless.{HNil, HList}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/10/12
 * Time: 12:09 AM
 */

abstract class TestScript extends App {

  // implicit that does Int 2 PimpedTCValue (RunTest): ->(Int) (which creates a PTestCriteriaRangeTuple, RunTest);
  //                                                   <>(Int) (which creates a PTestCriteriaRangeTuple, SkipTest);
  //                                                   unary_! (which creates a PTestCriteriaValueTuple, SkipTest)
  // PimpedTCRange exists
  // implicit that does ToggleFlag to PimpFlag
  // All three inherit from a common trait that gives: ~, ^^

  // case class ~

  //!
  //type Snowball <: HList

  case class PimpedRangeTuple(rangeTuple: TestRunningnessRange) extends CombinatorPimper(rangeTuple)
  case class PimpedToggleFlag(toggleFlag: TestCriteriaToggleFlag) extends CombinatorPimper(toggleFlag)

  case class PimpedValueTuple(valueTuple: TestRunningnessValue) extends CombinatorPimper(valueTuple) {
    def unary_! : PimpedValueTuple = PimpedValueTuple(TestRunningnessValue(valueTuple.guide, if (valueTuple.flag == RunTest) SkipTest else RunTest))
    def ^->(end: Int) : PimpedRangeTuple = PimpedRangeTuple(TestRunningnessRange(valueTuple.guide, end, RunTest))
    def ^<>(end: Int) : PimpedRangeTuple = PimpedRangeTuple(TestRunningnessRange(valueTuple.guide, end, SkipTest))
  }

  implicit def int2PTCVal(value: Int) : PimpedValueTuple = PimpedValueTuple(TestRunningnessValue(value, RunTest))
  
  implicit def flag2PTCFlag(flag: TestCriteriaToggleFlag) : PimpedToggleFlag = PimpedToggleFlag(flag)

  abstract class CombinatorPimper[T](val value: T) {
    def ^&[U](that: CombinatorPimper[U]) : HList = that.value :: value :: HNil
    def ^^ : List[T] = List(value)
  }

  implicit def hlistToPimpedHList(hl: HList) : PimpedHList = PimpedHList(hl)

  case class PimpedHList(hl: HList) {
    def ^&[U](that: CombinatorPimper[U]) : HList = that.value :: hl
    def ^^ = { List[TestCriteria]() }
    //def ^^ = hl.productIterator.toList.reverse.asInstanceOf[List[TestCriteria]] //!
  }

  //@ Can this be made better while still having `^^` return a supertype of the two values?
//  case class Snowball[+T, +U](_1: T, _2: U) extends ResultGiver[T] {
//
//    def ~[V](that: V) : Snowball[this.type, V] = Snowball(this, that)
//
//    // Builds a list forwards
//    def ^^ : List[X] = ^^! reverse
//
//    // Builds the list backwards
//    protected def ^^! : List[U] = {
//      _2 :: (_1 match {
//        case another: Snowball[_, _] => another ^^!
//        case alone                   => alone :: Nil
//      })
//    }
//
//  }

  //! BEGONE!
  implicit def strToCriteriaList(s: String) : List[TestCriteria] = {
    import tester.criteria.parser.TestCriteriaParser
    TestCriteriaParser.parseAll(TestCriteriaParser.criteria, s).get
  }

  def run[T <% List[TestCriteria]](criteria: T, pf: PathFinder[_ <: StepData]) {
    TestingCore(criteria, pf.asInstanceOf[PathFinder[StepData]], PathingTestCluster)
  }

}
