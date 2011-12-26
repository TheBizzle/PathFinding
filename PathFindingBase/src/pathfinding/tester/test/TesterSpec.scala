package pathfinding.tester.test

import dummies.DummyPathFinder
import org.scalatest.{FlatSpec, GivenWhenThen}
import pathfinding.tester.TestingCore
import pathfinding.tester.criteria._
import pathfinding.tester.exceptions._


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 1:08 PM
 */

class TesterSpec extends FlatSpec with GivenWhenThen {

    behavior of "A Tester"

    it should "be mad when it isn't given anything to run" in {

        given("nothing")
        val inList: List[TestCriteria[_]] = Nil

        when("the tester is invoked")
        then("NotRunningTestsException should be thrown")
        intercept[NotRunningTestsException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when you exclude all of the tests that you told it to run" in {
        
        given("a request to run test #1 and a request to skip test #1")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("NotRunningTestsException should be thrown")
        intercept[NotRunningTestsException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when you say that you don't want to run tests and then pass it test numbers to run" in {

        given("a SkipPathingTests flag and a request to run a pathing test")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaToggleFlag(SkipPathingTests))

        when("the tester is invoked")
        then("ContradictoryArgsException should be thrown")
        intercept[ContradictoryArgsException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when it's asked to run a single test of a test number that goes beyond the maximum test number" in {

        given("a single test of a test number that doesn't exist")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(100000, RunTest))

        when("the tester is invoked")
        then("InvalidTestNumberException should be thrown")
        intercept[InvalidTestNumberException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when it's asked to run tests on a range that goes beyond the maximum test number" in {

        given("a range that goes beyond the maximum test number")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 600000, RunRange))

        when("the tester is invoked")
        then("InvalidTestNumberException should be thrown")
        intercept[InvalidTestNumberException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    // @address Should be mad when an exclusion extends to a point that no inclusion goes to
    //          Write tests for containsOverlaps() and its caller
    //          Write tests for findMaxOfValues() and handleRanges()
    //          Write tests for new generateResultArray() and bucketAListOn_2() functions

    it should "be mad when a two of the same test are included redundantly" in {

        given("two of the same single, valid test")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a single test is redundantly included in a range" in {

        given("a valid range, and a valid single test (which was already included by the range)")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a test range encapsulates another test range" in {

        given("two valid test ranges (where one encapsulates the other)")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 4, RunRange), TestCriteriaRangeTuple(2, 3, RunRange))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a skip range encapsulates another skip range" in {

        given("two valid test ranges (where one encapsulates the other)")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaRangeTuple(2, 5, SkipRange), TestCriteriaRangeTuple(3, 4, SkipRange))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a test value is excluded unnecessarily" in {

        given("a valid range, and a valid single skip")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(2, 4, RunRange), TestCriteriaValueTuple(1, SkipTest))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a test range is excluded unnecessarily" in {

        given("a valid range, and a valid single test (which was already included by the range")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(4, 6, RunRange), TestCriteriaRangeTuple(1, 3, SkipRange))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a test range is extends to exclude unnecessarily" in {

        given("a valid range, and a valid single test (which was already included by the range")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 3, RunRange), TestCriteriaRangeTuple(5, 6, RunRange), TestCriteriaRangeTuple(2, 4, SkipRange))

        when("the tester is invoked")
        then("RedundancyException should be thrown")
        intercept[RedundancyException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

}