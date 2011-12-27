package tester

import criteria._
import exceptions._
import collection.immutable.{List, HashMap, Map}
import testcluster.TestCluster

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:27 PM
 */

object TestingCore {

    private[tester] val ArgKeyValue = "value"
    private[tester] val ArgKeyRange = "range"
    private[tester] val ArgKeyToggle = "toggle"

    // @address These existential types displease me...
    def apply(args: List[TestCriteria[_]], cluster: TestCluster[_]) {
        val argMap = sortArgLists(args)
        makeTestRunningDecisions(argMap, cluster)
    }

    private def makeTestRunningDecisions(argMap: Map[String, List[TestCriteria[_]]], cluster: TestCluster[_]) {

        val rawToggles = argMap.get(ArgKeyToggle).asInstanceOf[Option[List[TestCriteriaToggleFlag]]] match {
            case None    => throw new MysteriousDataException("OMG, what did you do?!")
            case Some(x) => x
        }

        val toggles = new TestToggleFlagWrapper(rawToggles)
        val wantsToRunPathing = assessPathingDesire(argMap)
        val (isTalkative, isRunningBaseTests, isSkippingPathingTests) = (toggles.get(Talkative), toggles.get(RunBaseTests), toggles.get(SkipPathingTests))

        if (isSkippingPathingTests)
            if(wantsToRunPathing)
                throw new ContradictoryArgsException("If you want skip the pathing tests, you should not specify pathing tests to run.")
            else if (!isRunningBaseTests)
                throw new NotRunningTestsException("You can't run the test suite if you're going to skip the pathing tests AND the base tests")

        val testsToRun = {
            if (!isSkippingPathingTests) {

                val valuesOption = argMap.get(ArgKeyValue).asInstanceOf[Option[List[TestCriteriaValueTuple]]]
                val rangesOption = argMap.get(ArgKeyRange).asInstanceOf[Option[List[TestCriteriaRangeTuple]]]

                val values = valuesOption  match {
                    case Some(x @ (h::t)) => sortCriteria(x)
                    case _                => Nil
                }

                val ranges = rangesOption match {
                    case Some(x @ (h::t)) => sortCriteria(x)
                    case _                => Nil
                }

                handleTestIntervals(values, ranges, cluster.getSize)

            }
            else
                Nil
        }

        cluster.runTests(testsToRun, isTalkative)

        if (isRunningBaseTests)
            runBaseTests()

    }

    // Basically, takes advantage of bucketing to quickly deal with test numbers and their test-ness/skip-ness
    // Calls an implicit conversion of List[RangeTuples] into List[List[ValueTuple]]s where it is called
    // To curry, or not to curry—that is the question
    private[tester] def generateResultArray(runRanges: List[TestCriteriaRangeTuple], runValues: List[TestCriteriaValueTuple],
                                            skipRanges: List[TestCriteriaRangeTuple], skipValues: List[TestCriteriaValueTuple], maxNum: Int) : Array[Boolean] = {
        val protoArr  = new Array[Boolean](maxNum + 1)
        val hemiArr   = applyValuesToArr(runRanges.flatten, protoArr)
        val modernArr = applyValuesToArr(runValues, hemiArr)
        val neoArr    = applyValuesToArr(skipRanges.flatten, modernArr)
        applyValuesToArr(skipValues, neoArr)
    }

    private[tester] def applyValuesToArr(values: List[TestCriteriaValueTuple], arr: Array[Boolean]) : Array[Boolean] = {
        values.foreach {
            x => {
                val isTesting = (x.criteria.flag == RunTest)
                if (arr(x.criteria.guide) != isTesting)
                    arr(x.criteria.guide) = isTesting
                else
                    throw new RedundancyException("Setting " + x.toString + " to" + {if (isTesting) " run " else " skip "} + "is unnecessary.")
            }
        }
        arr
    }

    // Expects values and ranges to both be sorted
    private[tester] def handleTestIntervals(values: List[TestCriteriaValueTuple], ranges: List[TestCriteriaRangeTuple], testCount: Int) : List[Int] = {

        val (testRanges, skipRanges, maxRangeVal) = handleRanges(ranges, testCount)
        val (testValues, skipValues, maxValueVal) = handleValues(values, testCount)
        val overallMax = if (maxValueVal > maxRangeVal) maxValueVal else maxRangeVal

        if (overallMax < 1)
            throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipPathingTests flag, instead!")

        val resultArr = generateResultArray(testRanges, testValues, skipRanges, skipValues, overallMax)
        val outList = { for ( i <- 0 until resultArr.size;
                              if (resultArr(i)) ) yield i }.toList

        if (!outList.isEmpty)
            outList
        else
            throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipPathingTests flag, instead!")

    }

    // Expects ranges to be sorted
    private[tester] def handleRanges(ranges: List[TestCriteriaRangeTuple], testCount: Int) : (List[TestCriteriaRangeTuple], List[TestCriteriaRangeTuple], Int) = {

        if (!ranges.isEmpty) {

            val (testList, skipList) = siftOutTestsAndSkips(ranges)

            val (testsHaveOverlap, firstTest, secondTest) = containsOverlaps(testList)
            if (testsHaveOverlap) throw new RedundancyException("Test list has an overlap between " + firstTest.get.toString + " and " + secondTest.get.toString)

            val (skipsHaveOverlap, firstSkip, secondSkip) = containsOverlaps(skipList)
            if (skipsHaveOverlap) throw new RedundancyException("Skip list has an overlap between " + firstSkip.get.toString + " and " + secondSkip.get.toString)

            val maxOfRanges = {
                if (!testList.isEmpty) {
                    val value = testList.last.criteria.guide._2
                    if (value <= testCount)
                        value
                    else
                        throw new InvalidTestNumberException("Test range " + testList.last.toString + " extends to a number for which there is no corresponding test")
                }
                else
                    0
            }

            (testList, skipList, maxOfRanges)

        }
        else
            (Nil, Nil, 0)

    }

    // Expects values to be sorted
    private[tester] def handleValues(values: List[TestCriteriaValueTuple], testCount: Int) : (List[TestCriteriaValueTuple], List[TestCriteriaValueTuple], Int) = {
        if (!values.isEmpty) {
            val (testList, skipList) = siftOutTestsAndSkips(values)
            val value = if (!testList.isEmpty) testList.last.criteria.guide else 0
            if (value <= testCount)
                (testList, skipList, value)
            else
                throw new InvalidTestNumberException("There is no test #" + values.last.criteria.guide)
        }
        else
            (Nil, Nil, 0)
    }

    private def runBaseTests() {
        // @address Probably just call execute() on a ScalaTest class or something
    }

    private[tester] def assessPathingDesire(argMap:  Map[String, List[TestCriteria[_]]]) : Boolean = {
        def hasPathingDesire(h: TestCriteria[_]) : Boolean = {
            h.asInstanceOf[TestCriteria[TestTuple[_,_]]].criteria.flag == RunTest
        }
        argMap(ArgKeyValue).exists(hasPathingDesire) || argMap(ArgKeyRange).exists(hasPathingDesire)
    }

    private[tester] def sortArgLists(args: List[TestCriteria[_]]) : Map[String, List[TestCriteria[_]]] = {
        def sortHelper(args: List[TestCriteria[_]], argMap: Map[String, List[TestCriteria[_]]]) : Map[String, List[TestCriteria[_]]] = {
            args match {
                case Nil  => argMap
                case h::t => {
                    val key = {
                        h match {
                            case x: TestCriteriaValueTuple => ArgKeyValue
                            case x: TestCriteriaRangeTuple => ArgKeyRange
                            case x: TestCriteriaToggleFlag => ArgKeyToggle
                        }
                    }
                    sortHelper(t, argMap + (key -> (h :: argMap(key))))
                }
            }
        }
        sortHelper(args, HashMap[String, List[TestCriteria[_]]](ArgKeyValue  -> List[TestCriteriaValueTuple](),
                                                                ArgKeyRange  -> List[TestCriteriaRangeTuple](),
                                                                ArgKeyToggle -> List[TestCriteriaToggleFlag]()))
    }

    private[tester] def sortCriteria[T <: TestCriteriaTuple[_, _] : Manifest](inList: List[T]) : List[T] = {
        inList match {
            case Nil    => Nil
            case h::t   => {
                val zippedList = inList.zipWithIndex                                        // Make (Tuple, Index) pairs
                val mediaryList = zippedList map ( x => (x._1.getKey, x._2) )               // Make (TupleKey, Index) pairs
                val sortedList = mediaryList.sortWith((a, b) => a._1 < b._1)                // Sort on TupleKey
                val sortedIndexes = sortedList map ( x => x._2 )                            // Just keep the sorted list of Indexes
                val bucketedArr = bucketAListOn_2(zippedList)                               // Make an array that maps Index->Tuple
                sortedIndexes.foldRight (List[T]()) ( (x, acc) => bucketedArr(x) :: acc )   // Fold the Indexes to generate an ordered list of Tuples
            }
        }
    }

    // This will break terribly if the ordered version of the set of all inList._2 values isn't equivalent to the set of all numbers 0 -> (inList.size - 1)
    // If you don't know what that means... you'll figure it out in due time.  Don't say that I didn't warn you, though.
    private[tester] def bucketAListOn_2[T : Manifest](inList: List[(T, Int)]) : Array[T] = {
        val buckets = new Array[T](inList.size)
        inList.foreach ( x => buckets(x._2) = x._1 )
        buckets
    }

    // Assumes the passed-in list to be sorted
    private[tester] def containsOverlaps(inList: List[TestCriteriaRangeTuple]) : (Boolean, Option[TestCriteriaRangeTuple], Option[TestCriteriaRangeTuple]) = {
        inList match {
            case h1::h2::t => {
                if (h1 intersects h2)
                    (true, Some(h1), Some(h2))
                else
                    containsOverlaps(inList.tail)
            }
            case _  => (false, None, None)
        }
    }

    private[tester] def siftOutTestsAndSkips[T <: TestCriteriaTuple[_, _] : Manifest](list: List[T]) : (List[T], List[T]) = {
        def siftHelper(inList: List[T], testList: List[T], skipList: List[T]) : (List[T], List[T]) = {
            inList match {
                case Nil                  => (testList.reverse, skipList.reverse)
                case h::t                 => {
                    val flag = h.criteria.flag
                    if (flag == RunTest)
                        siftHelper(t, h :: testList, skipList)
                    else if (flag == SkipTest)
                        siftHelper(t, testList, h :: skipList)
                    else
                        throw new UnexpectedTypeException("Unexpected type of TestRunningnessFlag!")   // EXPLODE!
                }
            }
        }
        siftHelper(list, List[T](), List[T]())
    }

}