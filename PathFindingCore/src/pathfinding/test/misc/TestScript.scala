package pathfinding.test.misc

import tester.criteria.{TestCriteriaValueTuple, TestCriteriaRangeTuple, TestRunningnessFlag}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/10/12
 * Time: 12:09 AM
 */

abstract class TestScript extends App {
    implicit def intIntFlagToRangeTuple(that: (Int, Int, TestRunningnessFlag)) : TestCriteriaRangeTuple = {
        import that._
        TestCriteriaRangeTuple(_1, _2, _3)
    }
    implicit def intFlagToValueTuple(that: (Int, TestRunningnessFlag)) : TestCriteriaValueTuple = {
        import that._
        TestCriteriaValueTuple(_1, _2)
    }
}