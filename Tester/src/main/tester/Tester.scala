package tester

import
  annotation.tailrec,
  collection.immutable.Map

import
  org.scalatest.Suite

import
  cluster._,
  criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:27 PM
 */

object Tester {

  private[tester] val ArgKeyValue  = "value"
  private[tester] val ArgKeyRange  = "range"
  private[tester] val ArgKeyToggle = "toggle"

  //@ Annoying compiler bug....  Fix this rubbish when the compiler gets fixed!
  def apply[T <: Testable, Subject <: TestSubject, Status <: ExecutionStatus, AnalysisFlags <: TestAnalysisFlagBundle, ResultFlags <: TestAnalysisResultBundle,
            TFConsBundle <: TestFuncConstructionBundle, TFunc <: TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], TCluster <: TestCluster[TFunc, Subject, TFConsBundle]]
           (args:      Seq[TestCriteria],
            testable:  T = null,
            cluster:   TCluster with TestCluster[TFunc with TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], Subject, TFConsBundle] = null,
            baseTests: Seq[Suite] = Seq[Suite]()) {
    val argMap = sortArgLists(args) //@ Uhh... wow.  How about, instead of wrapping these heterogenous `Seq`s up in a `Map`, I just put each as a member of one class...?
    makeTestRunningDecisions(argMap, testable, cluster, baseTests)
  }

  private def makeTestRunningDecisions[T <: Testable, Subject <: TestSubject, Status <: ExecutionStatus, AnalysisFlags <: TestAnalysisFlagBundle,
                                       ResultFlags <: TestAnalysisResultBundle, TFConsBundle <: TestFuncConstructionBundle,
                                       TFunc <: TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], TCluster <: TestCluster[TFunc, Subject, TFConsBundle]]
                                       (argMap: Map[String, Seq[TestCriteria]],
                                        testable: T,
                                        cluster: TCluster with TestCluster[TFunc with TestFunction[T, Subject, Status, AnalysisFlags, ResultFlags], Subject, TFConsBundle],
                                        baseTests: Seq[Suite]) {

    val rawToggles = argMap.get(ArgKeyToggle).asInstanceOf[Option[Seq[TestCriteriaToggleFlag]]] getOrElse (throw new MysteriousDataException("OMG, what did you do?!"))

    val toggles = new TestToggleFlagManager(rawToggles)
    val wantsToRunExternals = assessExternalityDesire(argMap)
    val (isTalkative, isRunningBaseTests, isSkippingExternalTests, isStacktracing) = (toggles.get(Talkative), toggles.get(RunBaseTests), toggles.get(SkipExternalTests), toggles.get(StackTrace))

    if (isSkippingExternalTests && wantsToRunExternals)
      throw new ContradictoryArgsException("If you want skip the external tests, you should not specify external tests to run.")

    if (!wantsToRunExternals && !isRunningBaseTests)
      throw new NotRunningTestsException("You can't run the test suite if you're going to skip the external tests AND the base tests")

    if (!isSkippingExternalTests && wantsToRunExternals) {

      val valuesOption = argMap.get(ArgKeyValue).asInstanceOf[Option[Seq[TestRunningnessValue]]]
      val rangesOption = argMap.get(ArgKeyRange).asInstanceOf[Option[Seq[TestRunningnessRange]]]

      val values = valuesOption map (sortCriteria(_)) getOrElse(Seq())
      val ranges = rangesOption map (sortCriteria(_)) getOrElse(Seq())

      val testFlagPairs  = Seq(isTalkative) zip Seq[TestToggleFlag](Talkative)
      val testToggles    = testFlagPairs collect { case (true, x) => x }
      val testFlagBundle = new TestFuncFlagBundle(testToggles)
      val testsToRun     = handleTestIntervals(values, ranges, cluster.getSize)

      runTests(cluster.getTestsToRun(testsToRun), testable, testFlagBundle, isStacktracing)

    }

    if (isRunningBaseTests)
      runBaseTests(baseTests)

  }

  private def runTests[T <: Testable, TFunc <: TestFunction[T, _, _, _, _]](tests: Seq[TFunc], testable: T, flags: TestFuncFlagBundle, isStacktracing: Boolean) {

    def successStr(testNumber: Int) = "Test number " + testNumber + " was a success."
    def failureStr(testNumber: Int) = "Test number " + testNumber + " failed miserably!"

    tests foreach {
      case test =>
        try {
          val f = if (test.shouldSucceed == test(testable, flags)) successStr _ else failureStr _
          println(f(test.testNum))
        }
        catch {
          case e: Exception =>
            println("Test number " + test.testNum + " failed with an exception (" + e.getClass + ").")
            if (isStacktracing) println("\n" + e.getStackTraceString)
        }
    }

  }

  // Basically, takes advantage of bucketing to quickly deal with test numbers and their test-ness/skip-ness
  // Calls an implicit conversion of Seq[RangeTuples] into Seq[Seq[ValueTuple]]s where it is called
  private[tester] def generateResultArray(runRanges: Seq[TestRunningnessRange], runValues: Seq[TestRunningnessValue],
                                          skipRanges: Seq[TestRunningnessRange], skipValues: Seq[TestRunningnessValue], maxNum: Int) : Array[Boolean] = {
    val arr  = new Array[Boolean](maxNum + 1)
    Seq(runRanges.flatten, runValues, skipRanges.flatten, skipValues) foreach (applyValuesToArr(_, arr))
    arr
  }

  private[tester] def applyValuesToArr(values: Seq[TestRunningnessValue], arr: Array[Boolean]) : Array[Boolean] = {
    values foreach {
      x =>
        val isTesting = isIncludingTest(x)
        if (arr(x.guide) != isTesting)
          arr(x.guide) = isTesting
        else
          throw new RedundancyException("Setting " + x.toString + " to" + ( if (isTesting) " run " else " skip " ) + "is unnecessary.")
    }
    arr
  }

  // Expects values and ranges to both be sorted
  private[tester] def handleTestIntervals(values: Seq[TestRunningnessValue], ranges: Seq[TestRunningnessRange], testCount: Int) : Seq[Int] = {

    val (testRanges, skipRanges, maxRangeVal) = handleRanges(ranges, testCount)
    val (testValues, skipValues, maxValueVal) = handleValues(values, testCount)
    val overallMax = if (maxValueVal > maxRangeVal) maxValueVal else maxRangeVal

    if (overallMax < 1)
      throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipExternalTests flag, instead!")

    val resultArr = generateResultArray(testRanges, testValues, skipRanges, skipValues, overallMax)
    val out = resultArr.zipWithIndex collect { case (true, x) => x } toSeq

    if (!out.isEmpty)
      out
    else
      throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipExternalTests flag, instead!")

  }

  // Expects ranges to be sorted
  private[tester] def handleRanges(ranges: Seq[TestRunningnessRange], testCount: Int) : (Seq[TestRunningnessRange], Seq[TestRunningnessRange], Int) = {

    if (!ranges.isEmpty) {

      val (tests, skips) = ranges.partition(isIncludingTest)

      val (testsHaveOverlap, firstTest, secondTest) = containsOverlaps(tests)
      if (testsHaveOverlap) throw new RedundancyException("Test list has an overlap between " + firstTest.get.toString + " and " + secondTest.get.toString)

      val (skipsHaveOverlap, firstSkip, secondSkip) = containsOverlaps(skips)
      if (skipsHaveOverlap) throw new RedundancyException("Skip list has an overlap between " + firstSkip.get.toString + " and " + secondSkip.get.toString)

      val maxOfRanges = {
        if (!tests.isEmpty) {
          val value = tests.last.guide._2
          if (value <= testCount)
            value
          else
            throw new InvalidTestNumberException("Test range " + tests.last.toString + " extends to a number for which there is no corresponding test.  " +
                                                 "Min is 1.  Max is " + testCount + ".")
        }
        else
          0
      }

      (tests, skips, maxOfRanges)

    }
    else (Seq(), Seq(), 0)

  }

  // Expects values to be sorted
  private[tester] def handleValues(values: Seq[TestRunningnessValue], testCount: Int) : (Seq[TestRunningnessValue], Seq[TestRunningnessValue], Int) = {
    if (!values.isEmpty) {
      val (tests, skips) = values.partition(isIncludingTest)
      val value          = if (!tests.isEmpty) tests.last.guide else 0
      if (value <= testCount)
        (tests, skips, value)
      else
        throw new InvalidTestNumberException("There is no test #" + values.last.guide)
    }
    else (Seq(), Seq(), 0)
  }

  private def runBaseTests(baseTests: Seq[Suite]) {
    baseTests foreach { x => print("\n"); x.execute(stats = true) }
  }

  private[tester] def assessExternalityDesire(argMap:  Map[String, Seq[TestCriteria]]) : Boolean =
    argMap(ArgKeyValue).asInstanceOf[Seq[TestRunningnessValue]].exists(isIncludingTest) || argMap(ArgKeyRange).asInstanceOf[Seq[TestRunningnessRange]].exists(isIncludingTest)

  private[tester] def sortArgLists(args: Seq[TestCriteria]) : Map[String, Seq[TestCriteria]] = {

    val baseMap = Map(ArgKeyValue  -> Seq[TestRunningnessValue](),
                      ArgKeyRange  -> Seq[TestRunningnessRange](),
                      ArgKeyToggle -> Seq[TestCriteriaToggleFlag]())

    val argMap = args.groupBy {
      case _: TestRunningnessValue   => ArgKeyValue
      case _: TestRunningnessRange   => ArgKeyRange
      case _: TestCriteriaToggleFlag => ArgKeyToggle
      case x                         => throw new MysteriousDataException("How did THAT get in there...? : " + x)
    }

    baseMap ++ argMap

  }

  private[tester] def sortCriteria[T <: TestRunningnessCriteria[_, _] : Manifest](criteria: Seq[T]) : Seq[T] = criteria sortBy (_.getKey)

  // Assumes the passed-in ranges to be sorted (could be written more idiomatically, but I would then lose my pointless optimization!)
  @tailrec
  private[tester] def containsOverlaps(ranges: Seq[TestRunningnessRange]) : (Boolean, Option[TestRunningnessRange], Option[TestRunningnessRange]) = {
    ranges.toList match {
      case r1 :: r2 :: tail =>
        if (r1 intersects r2)
          (true, Some(r1), Some(r2))
        else
          containsOverlaps(r2 :: tail)
      case _  => (false, None, None)
    }
  }

  private def isIncludingTest(tuple: TestRunningnessCriteria[_, _]) = tuple.flag == RunTest

}
