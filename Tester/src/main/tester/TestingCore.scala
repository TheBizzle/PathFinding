package tester

import criteria._
import exceptions._
import collection.immutable.{List, Map}
import annotation.tailrec
import org.scalatest.Suite
import testanalyzer.{TestAnalysisResultBundle, ExecutionStatus, TestAnalysisFlagBundle}
import testcluster._
import testfunction.{TestFuncConstructionBundle, TestFunction, TestFuncFlagBundle}

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


    def apply[T <: Testable, Subject <: TestSubject, Status <: ExecutionStatus, AnalysisFlags <: TestAnalysisFlagBundle, ResultFlags <: TestAnalysisResultBundle,
              TFConsBundle <: TestFuncConstructionBundle, TFunc <: TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], TCluster <: TestCluster[TFunc, Subject, TFConsBundle]]
             (args: List[TestCriteria[_]],
              testable: T = null,
              cluster: TCluster with TestCluster[TFunc with TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], Subject, TFConsBundle] = null,
              baseTests: Seq[Suite] = Seq[Suite]()) {

        val argMap = sortArgLists(args)
        makeTestRunningDecisions(argMap, testable, cluster, baseTests)
        
    }

    private def makeTestRunningDecisions[T <: Testable, Subject <: TestSubject, Status <: ExecutionStatus, AnalysisFlags <: TestAnalysisFlagBundle,
                                         ResultFlags <: TestAnalysisResultBundle, TFConsBundle <: TestFuncConstructionBundle,
                                         TFunc <: TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], TCluster <: TestCluster[TFunc, Subject, TFConsBundle]]
                                        (argMap: Map[String, List[TestCriteria[_]]],
                                         testable: T,
                                         cluster: TCluster with TestCluster[TFunc with TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], Subject, TFConsBundle],
                                         baseTests: Seq[Suite]) {

        val rawToggles = argMap.get(ArgKeyToggle).asInstanceOf[Option[List[TestCriteriaToggleFlag]]].getOrElse(throw new MysteriousDataException("OMG, what did you do?!"))

        val toggles = new TestToggleFlagWrapper(rawToggles)
        val wantsToRunExternals = assessExternalityDesire(argMap)
        val (isTalkative, isRunningBaseTests, isSkippingExternalTests, isStacktracing) = (toggles.get(Talkative), toggles.get(RunBaseTests), toggles.get(SkipExternalTests), toggles.get(StackTrace))

        if (isSkippingExternalTests && wantsToRunExternals)
            throw new ContradictoryArgsException("If you want skip the external tests, you should not specify external tests to run.")

        if (!wantsToRunExternals && !isRunningBaseTests)
            throw new NotRunningTestsException("You can't run the test suite if you're going to skip the external tests AND the base tests")

        if (!isSkippingExternalTests && wantsToRunExternals) {

            val valuesOption = argMap.get(ArgKeyValue).asInstanceOf[Option[List[TestCriteriaValueTuple]]]
            val rangesOption = argMap.get(ArgKeyRange).asInstanceOf[Option[List[TestCriteriaRangeTuple]]]

            val values = valuesOption map (sortCriteria(_)) getOrElse(Nil)
            val ranges = rangesOption map (sortCriteria(_)) getOrElse(Nil)

            val testFlagPairs = List(isTalkative) zip List[TestToggleFlag](Talkative)
            val testToggles = testFlagPairs collect { case (true, x) => x }
            val testFlagBundle = new TestFuncFlagBundle(testToggles)
            val testsToRun = handleTestIntervals(values, ranges, cluster.getSize)

            runTests(cluster.getTestsToRun(testsToRun), testable, testFlagBundle, isStacktracing)

        }

        if (isRunningBaseTests)
            runBaseTests(baseTests)

    }

    private def runTests[T <: Testable, TFunc <: TestFunction[T, _, _, _, _]]
                        (tests: List[TFunc], testable: T, flags: TestFuncFlagBundle, isStacktracing: Boolean) {

        def successStr(testNumber: Int) = "Test number " + testNumber + " was a success."
        def failureStr(testNumber: Int) = "Test number " + testNumber + " failed miserably!"

        tests foreach {
            case test =>
            try {
                val result = test(testable, flags)
                if (test.shouldSucceed == result) println(successStr(test.testNum))
                else                              println(failureStr(test.testNum))
            }
            catch {
                case e: Exception =>
                    println("Test number " + test.testNum + " failed with an exception (" + e.getClass + ").")
                    if (isStacktracing) println("\n" + e.getStackTraceString)
            }
        }
        
    }

    // Basically, takes advantage of bucketing to quickly deal with test numbers and their test-ness/skip-ness
    // Calls an implicit conversion of List[RangeTuples] into List[List[ValueTuple]]s where it is called
    private[tester] def generateResultArray(runRanges: List[TestCriteriaRangeTuple], runValues: List[TestCriteriaValueTuple],
                                            skipRanges: List[TestCriteriaRangeTuple], skipValues: List[TestCriteriaValueTuple], maxNum: Int) : Array[Boolean] = {
        val arr  = new Array[Boolean](maxNum + 1)
        List(runRanges.flatten, runValues, skipRanges.flatten, skipValues) foreach (applyValuesToArr(_, arr))
        arr
    }

    private[tester] def applyValuesToArr(values: List[TestCriteriaValueTuple], arr: Array[Boolean]) : Array[Boolean] = {
        values foreach {
            case x =>
                val isTesting = isIncludingTest(x)
                if (arr(x.criteria.guide) != isTesting)
                    arr(x.criteria.guide) = isTesting
                else
                    throw new RedundancyException("Setting " + x.toString + " to" + ( if (isTesting) " run " else " skip " ) + "is unnecessary.")
        }
        arr
    }

    // Expects values and ranges to both be sorted
    private[tester] def handleTestIntervals(values: List[TestCriteriaValueTuple], ranges: List[TestCriteriaRangeTuple], testCount: Int) : List[Int] = {

        val (testRanges, skipRanges, maxRangeVal) = handleRanges(ranges, testCount)
        val (testValues, skipValues, maxValueVal) = handleValues(values, testCount)
        val overallMax = if (maxValueVal > maxRangeVal) maxValueVal else maxRangeVal

        if (overallMax < 1)
            throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipExternalTests flag, instead!")

        val resultArr = generateResultArray(testRanges, testValues, skipRanges, skipValues, overallMax)
        val outList = resultArr.zipWithIndex collect { case (true, x) => x } toList

        if (!outList.isEmpty)
            outList
        else
            throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipExternalTests flag, instead!")

    }

    // Expects ranges to be sorted
    private[tester] def handleRanges(ranges: List[TestCriteriaRangeTuple], testCount: Int) : (List[TestCriteriaRangeTuple], List[TestCriteriaRangeTuple], Int) = {

        if (!ranges.isEmpty) {

            val (testList, skipList) = ranges.partition(isIncludingTest)

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
                        throw new InvalidTestNumberException("Test range " + testList.last.toString + " extends to a number for which there is no corresponding test.  " +
                                                             "Min is 1.  Max is " + testCount + ".")
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
            val (testList, skipList) = values.partition(isIncludingTest)
            val value = if (!testList.isEmpty) testList.last.criteria.guide else 0
            if (value <= testCount)
                (testList, skipList, value)
            else
                throw new InvalidTestNumberException("There is no test #" + values.last.criteria.guide)
        }
        else
            (Nil, Nil, 0)
    }

    private def runBaseTests(baseTests: Seq[Suite]) {
        baseTests foreach { case x => print("\n"); x.execute(stats = true) }
    }

    private[tester] def assessExternalityDesire(argMap:  Map[String, List[TestCriteria[_]]]) : Boolean = {
        argMap(ArgKeyValue).asInstanceOf[List[TestCriteriaValueTuple]].exists(isIncludingTest) || argMap(ArgKeyRange).asInstanceOf[List[TestCriteriaRangeTuple]].exists(isIncludingTest)
    }

    private[tester] def sortArgLists(args: List[TestCriteria[_]]) : Map[String, List[TestCriteria[_]]] = {

        val baseMap = Map(ArgKeyValue  -> List[TestCriteriaValueTuple](),
                          ArgKeyRange  -> List[TestCriteriaRangeTuple](),
                          ArgKeyToggle -> List[TestCriteriaToggleFlag]())

        val argMap = args.groupBy {
            case _: TestCriteriaValueTuple => ArgKeyValue
            case _: TestCriteriaRangeTuple => ArgKeyRange
            case _: TestCriteriaToggleFlag => ArgKeyToggle
            case _                         => throw new MysteriousDataException("How did THAT get in there...?")
        }

        baseMap ++ argMap

    }

    private[tester] def sortCriteria[T <: TestCriteriaTuple[_, _] : Manifest](inList: List[T]) : List[T] = {
        inList sortBy (_.getKey)
    }

    // Assumes the passed-in list to be sorted
    @tailrec
    private[tester] def containsOverlaps(inList: List[TestCriteriaRangeTuple]) : (Boolean, Option[TestCriteriaRangeTuple], Option[TestCriteriaRangeTuple]) = {
        inList match {
            case h1::h2::_ =>
                if (h1 intersects h2)
                    (true, Some(h1), Some(h2))
                else
                    containsOverlaps(inList.tail)
            case _  => (false, None, None)
        }
    }

    private def isIncludingTest(tuple: TestCriteriaTuple[_,_]) : Boolean = {
        tuple.criteria.flag == RunTest
    }

}
