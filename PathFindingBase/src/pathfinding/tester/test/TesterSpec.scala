package pathfinding.tester.test

import dummies.DummyPathFinder
import org.scalatest.{FlatSpec, GivenWhenThen}
import pathfinding.tester.TestingCore
import pathfinding.tester.criteria._
import java.security.InvalidParameterException


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
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when you exclude all of the tests that you told it to run" in {
        
        given("a request to run test #1 and a request to skip test #1")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when you say that you don't want to run tests and then pass it test numbers to run" in {

        given("a SkipPathingTests flag and a request to run a pathing test")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaToggleFlag(SkipPathingTests))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when it's asked to run a single test of a test number that goes beyond the maximum test number" in {

        given("a single test of a test number that doesn't exist")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(100000, RunTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when it's asked to run tests on a range that goes beyond the maximum test number" in {

        given("a range that goes beyond the maximum test number")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 600000, RunRange))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a value is excluded unnecessarily" in {

        given("a single valid inclusion and a single invalid exclusion")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(2, SkipTest), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a range is excluded unnecessarily" in {

        given("a valid range to include and an invalid range to exclude")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaRangeTuple(3, 4, SkipRange))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when passed a fully-encapsulated range for double inclusion" in {

        given("two valid ranges of inclusions, where one fully encapsulates the other")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 4, RunRange), TestCriteriaRangeTuple(2, 3, RunRange))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when passed a fully-encapsulated range for double exclusion" in {

        given("one valid range of inclusions and two valid ranges of exclusions, where one exclusion range fully encapsulates the other")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, RunRange), TestCriteriaRangeTuple(2, 5, SkipRange), TestCriteriaRangeTuple(3, 4, SkipRange))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when passed a fully-encapsulated range for single exclusion" in {

        given("one valid range of inclusion and one valid range for exclusion, where the exclusion range fully encapsulates the inclusion range")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 6, SkipRange), TestCriteriaRangeTuple(2, 5, RunRange))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a two of the same test are included redundantly" in {

        given("two of the same single, valid test")
        val inList = List[TestCriteria[_]](TestCriteriaValueTuple(1, RunTest), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a single test is redudantly included in a range" in {

        given("a valid range, and a valid single test (which was already included by the range")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 2, RunRange), TestCriteriaValueTuple(1, RunTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when two of the same test are excluded redundantly" in {

        given("a valid range of inclusions, and two of the same single, valid skip")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 4, RunRange), TestCriteriaValueTuple(1, SkipTest), TestCriteriaValueTuple(1, SkipTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

    it should "be mad when a single test is redudantly excluded in a range" in {

        given("a valid range of tests, a valid range of skips, and a valid single test skip (which was already included by the range)")
        val inList = List[TestCriteria[_]](TestCriteriaRangeTuple(1, 5, RunRange), TestCriteriaRangeTuple(2, 4, SkipRange), TestCriteriaValueTuple(2, SkipTest))

        when("the tester is invoked")
        then("InvalidParameterException should be thrown")
        intercept[InvalidParameterException] {
            TestingCore(inList, DummyPathFinder)
        }

    }

}